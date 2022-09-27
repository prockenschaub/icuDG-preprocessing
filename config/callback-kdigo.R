library(data.table)
library(assertthat)
library(ricu)

kdigo <- function(..., keep_components = FALSE, interval = NULL) {
  
  cnc <- c("crea_stg", "urine_stg")
  res <- ricu:::collect_dots(cnc, interval, ...)
  crea_stg <- res[["crea_stg"]]
  urine_stg <- res[["urine_stg"]]
  
  id <- id_vars(crea_stg)
  ind <- index_var(crea_stg)
  
  res <- merge(crea_stg, urine_stg, by = c(id, ind), all = TRUE)
  res[, kdigo := pmax(crea_stg, urine_stg, na.rm = TRUE)]
  
  if (!keep_components) {
    cols_rm <- c("crea_stg", "urine_stg")
    res <- rm_cols(res, cols_rm, skip_absent = TRUE, by_ref = TRUE)
  }
  res
}

crea_stg <- function(..., keep_components = FALSE, interval = NULL) {
  cnc <- c("crea")
  crea <- ricu:::collect_dots(cnc, interval, ...)
  
  id <- id_vars(crea)
  ind <- index_var(crea)
  
  min_over_period <- function(dur = hours(1L)) {
    cdur <- as.character(dur)
    summ <- slide(
      crea, 
      list(crea = min(get("crea"), na.rm = TRUE)), 
      dur,
      left_closed = FALSE
    )
    rename_cols(summ, paste0("crea_", cdur, "hr"), "crea")
  }
  
  res <- lapply(hours(2 * 24, 7 * 24), min_over_period)
  res <- merge_lst(c(list(crea), res))
  res[, crea_stg := fcase(
      crea >= 3 * crea_168hr                         , 3L,
      crea >= 4 & 
        (crea_48hr <= 3.7 | crea >= 1.5 * crea_168hr), 3L,
      crea >= 2 * crea_168hr                         , 2L,
      crea >= crea_48hr + 0.3                        , 1L,
      crea >= 1.5 * crea_168hr                       , 1L,
      default = 0L
  )]
  
  cols_rm <- c("crea_48hr", "crea_168hr")
  if (!keep_components) {
    cols_rm <- c(cols_rm, "crea")
  }
  res <- rm_cols(res, cols_rm, skip_absent = TRUE, by_ref = TRUE)
  res
}


urine_stg <- function(..., keep_components = FALSE, interval = NULL) {
  cnc <- c("urine", "weight")
  res <- ricu:::collect_dots(cnc, interval, ...)
  urine <- res[["urine"]]
  weight <- res[["weight"]]
  
  id <- id_vars(urine)
  ind <- index_var(urine)
  
  rate_over_period <- function(dur = hours(1L)) {
    cdur <- as.character(dur)
    summ <- slide(
      urine, 
      list(
        urine = sum(get("urine"), na.rm = TRUE), 
        urine_tm = get(!!ind)[.N] - get(!!ind)[1] + 1 # Note: this is conservative and will underestimate tm if there are long gaps between measurements
      ), 
      dur,
      left_closed = FALSE
    )
    summ[weight, urine_rt := urine / weight / as.numeric(urine_tm), on = c(id)]
    nms <- paste0(c("urine", "urine_tm", "urine_rt"), "_", cdur, "hr")
    rename_cols(summ, nms, c("urine", "urine_tm", "urine_rt"))
  }
  
  res <- lapply(hours(6L, 12L, 24L), rate_over_period)
  res <- merge_lst(res)
  res[, urine_stg := fcase(
    get(ind) < 6                             , 0L,
    urine_tm_24hr >= 12 & urine_rt_24hr < 0.3, 3L,
    urine_tm_12hr >= 6  & urine_rt_12hr == 0 , 3L,
    urine_tm_12hr >= 6  & urine_rt_12hr < 0.5, 2L,
    urine_tm_6hr  >= 3  & urine_rt_6hr  < 0.5, 1L,
    default = 0L
  )]
  
  cols_rm <- c(
    "urine_6hr", "urine_12hr", "urine_24hr",
    "urine_tm_6hr", "urine_tm_12hr", "urine_tm_24hr",
    "urine_rt_6hr", "urine_rt_12hr", "urine_rt_24hr"
  )
  if (!keep_components) {
    cols_rm <- c(cols_rm, "urine", "weight")
  }
  res <- rm_cols(res, cols_rm, skip_absent = TRUE, by_ref = TRUE)
  res
}


