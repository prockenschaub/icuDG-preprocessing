
cummax_difftime <- function(x){
  # TODO: change to allow other intervals than hours
  as.difftime(cummax(as.numeric(x)), units = "hours")
}
lead <- function(x) {
  data.table::shift(x, type="lead")
}

abx_cont <- function(..., abx_win = hours(72L), abx_max_gap = hours(24L), keep_components = FALSE, interval = NULL) {
  cnc <- c("abx_duration", "death_icu")
  res <- ricu:::collect_dots(cnc, interval, ...)
  abx <- res[["abx_duration"]]
  death_icu <- res[["death_icu"]]
  
  aid <- id_vars(abx)
  aind <- index_var(abx)
  adur <- dur_var(abx)
  did <- id_vars(death_icu)
  dind <- index_var(death_icu)
  
  abx <- as_ts_tbl(abx)
  abx <- abx[, .(dur_var = max(get(adur))), by = c(aid, aind)]
  death_icu <- death_icu[death_icu == TRUE]
  abx_death <- merge(abx, death_icu, by.x = aid, by.y = did, all.x = TRUE)
  
  res <- slide(
    # Only look at antibiotic records that are recorded before the time of death
    abx_death[is.na(get(dind)) | get(aind) <= get(dind)], 
    .(
    # Calculate the maximum gap between two administrations for the next `abx_win` hours
    # as follows: 
    #
    # 1. get the administration time of the next antibiotic:
    #        lead(get(aind))
    # 2. this isn't defined for the last (.N-th) time within the window, so remove that 
    #        lead(get(aind))[-.N]:
    # 3. replace the last time with either 
    #   a) the time of death: 
    #        get(dind)
    #   b) the first antibiotic time in the window (=current antibiotic we are looking at)
    #      plus the window lenght
    #        get(aind)[1] + abx_win
    #    whichever is earlier
    # 4. subtract from it the latest time that any previous antibiotic was stopped 
    #        cummax_difftime(get(aind) + dur_var)
    #    this is the gap
    # 5. take the maximum gap calculated this way for this window
    # 6. repeat for all possible windows
      max_gap = max(
        c(lead(get(aind))[-.N], min(c(get(dind), get(aind)[1] + abx_win), na.rm = TRUE)) - 
        cummax_difftime(get(aind) + dur_var)
      )
    ), 
    before = hours(0L), # we always start from the current antibiotic and look `abx_win` in the future
    after = abx_win
  )
  
  res <- res[max_gap <= abx_max_gap]
  res[, c("abx_cont", "max_gap") := .(TRUE, NULL)]
  res
}


susp_inf_alt <- function(..., abx_count_win = hours(24L), abx_min_count = 1L, 
              positive_cultures = FALSE, si_mode = c("and", "or", "abx", "samp"), 
              abx_win = hours(24L), samp_win = hours(72L), 
              by_ref = TRUE, keep_components = FALSE, interval = NULL) 
{
  cnc <- c("abx_cont", "samp")
  res <- ricu:::collect_dots(cnc, interval, ...)
  abx_cont <- res[["abx_cont"]]
  samp <- res[['samp']]
  
  # make `abx_cont` look like abx to pass on to the original ricu::susp_inf
  rename_cols(abx_cont, "abx", "abx_cont", by_ref = TRUE)
  
  # pass the rest of the calculations to ricu::susp_inf
  res <- ricu::susp_inf(
    abx = abx_cont,
    samp = samp,
    abx_count_win = abx_count_win, 
    abx_min_count = abx_min_count, 
    positive_cultures = positive_cultures, 
    si_mode = si_mode, 
    abx_win = abx_win, 
    samp_win = samp_win, 
    by_ref = by_ref, 
    keep_components = keep_components, 
    interval = interval
  )
  rename_cols(res, "susp_inf_alt", "susp_inf", by_ref = TRUE)
  res
}


sep3_alt <- function (..., si_window = c("first", "last", "any"), delta_fun = delta_cummin, 
          sofa_thresh = 2L, si_lwr = hours(48L), si_upr = hours(24L), 
          keep_components = FALSE, interval = NULL) 
{
  cnc <- c("sofa", "susp_inf_alt")
  res <- ricu:::collect_dots(cnc, interval, ...)
  sofa <- res[["sofa"]]
  susp <- res[["susp_inf_alt"]]

  # make `susp_inf_alt` look like susp_inf to pass on to the original ricu::sep3
  rename_cols(susp, "susp_inf", "susp_inf_alt", by_ref = TRUE)
  
  # pass the rest of the calculations to ricu::susp_inf
  res <- ricu::sep3(
    sofa = sofa,
    susp_inf = susp,
    si_window = si_window, 
    delta_fun = delta_fun, 
    sofa_thresh = sofa_thresh, 
    si_lwr = si_lwr, 
    si_upr = si_upr,
    keep_components = keep_components, 
    interval = interval
  )
  rename_cols(res, "sep3_alt", "sep3", by_ref = TRUE)
  res
}
  