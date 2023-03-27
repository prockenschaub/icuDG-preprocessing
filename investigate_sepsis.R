library(tidyverse)

load_sepsis_with_components <- function(src) {
  sofa <- load_concepts(dict['sofa'], src, keep_components = TRUE)
  ricu::rename_cols(sofa, "time", index_var(sofa), by_ref = TRUE)
  
  if (src %in% c("eicu", "eicu_demo", "hirid")) {
    susp <- load_concepts(dict['susp_inf'], src, abx_min_count = 2L, si_mode = "abx", keep_components = TRUE)
  } else {
    susp <- load_concepts(dict['susp_inf'], src, keep_components = TRUE)
  }
  
  ricu::rename_cols(susp, "time", index_var(susp), by_ref = TRUE)
  sep <- sep3(sofa = sofa, susp_inf = susp)
  ricu::rename_cols(sep, "time", index_var(sep), by_ref = TRUE)
  
  id <- id_vars(sep)
  ind <- index_var(sep)
  joint <- purrr::reduce(list(sep, sofa, susp), full_join, by = c(id, ind))
  joint[order(get(id), get(ind))]
}

limit_to_cohort <- function(df, src) {
  # Load cohort file
  conf <- ricu:::read_json("config.json")
  path <- file.path(conf$output_dir, "sepsis", src)
  outc <- arrow::read_parquet(file.path(path, "outc.parquet"))
  has_sep <- outc[, .(has_sep = any(label)), by = "stay_id"][has_sep == TRUE]
  
  # subset 
  join_cols <- c(id_vars(has_sep))
  names(join_cols) <- id_var(df)
  df <- df %>% semi_join(has_sep, by = join_cols)
  df <- df[get(index_var(df)) <= hours(193L)]
  df
}

process_components <- function(df) {
  id <- id_vars(df)
  ind <- index_var(df)
  
  # Which SOFA increase led to the sepsis definition? What other SOFA were present?
  for (col in names(df)[str_detect(names(df), "sofa_")]) {
    df[, str_c(col, "_inc") := get(id) == shift(get(id)) & get(col) > shift(get(col))]
    df[, str_c(col, "_bef") := get(col) > 0 & !get(str_c(col, "_inc"))]
  }
  
  # Was a sample taken or an antibiotic given before?
  if (!"samp_time" %in% names(df)) {
    df[, samp_time := NA_real_]
    df[, abx_time := ifelse(!is.na(susp_inf), time, NA)]
  }
  
  df[, samp_time := data.table::nafill(data.table::nafill(samp_time, type = 'locf'), type = 'nocb'), by = c(id)]
  df[, samp_bef := samp_time < get(ind)]
  df[, abx_time := data.table::nafill(data.table::nafill(abx_time, type = 'locf'), type = 'nocb'), by = c(id)]
  df[, abx_bef := abx_time < get(ind)]
  
  df
}

calculate_measures <- function(df) {
  ini <- df[time >= 0][, .SD[1], by = c(id_vars(df))]
  fin <- df[sep3 == TRUE][, .SD[1], by = c(id_vars(df))]
  
  list(
    sofa_increase = fin[, map_dbl(.SD, mean, na.rm = TRUE), .SDcols = str_detect(names(fin), "comp_inc")],
    sofa_concomit = fin[, map_dbl(.SD, mean, na.rm = TRUE), .SDcols = str_detect(names(fin), "comp_bef")],
    sofa_adm = ini$sofa %>% table() %>% prop.table(),
    sofa_abs = fin$sofa %>% table() %>% prop.table(),
    samp_before = fin$samp_bef %>% mean(),
    abx_before = fin$abx_bef %>% mean()
  )
}

run_for_src <- function(src) {
  data <- load_sepsis_with_components(src)
  data_cohort <- process_components(copy(data))
  data_cohort <- limit_to_cohort(data_cohort, src)
  
  calculate_measures(data_cohort)
}

stats <- list()

for (src in c("aumc", "eicu", "hirid", "mimic")) {
  stats[[src]] <- run_for_src(src)
}

stats_df <- list()

for (n in names(stats[[1]])) {
  vals <- map(stats, n)
  if (length(vals[[1]]) > 1) {
    stats_df[[n]] <- map2(vals, names(vals), ~ tibble(index = names(.x), vals = as.numeric(.x), src = .y)) %>% bind_rows()
  } else {
    stats_df[[n]] <- map2(vals, names(vals), ~ tibble(vals = as.numeric(.x), src = .y)) %>% bind_rows()
  }
}


ggplot(stats_df$sofa_increase, aes(index, vals, group = src, colour = src)) + 
  geom_line() + 
  scale_x_discrete(
    limits = str_c("sofa_", c("cardio", "cns", "coag", "liver", "renal", "resp"), "_comp_inc"),
    labels = c("cardio", "cns", "coag", "liver", "renal", "resp")
  ) + 
  labs(
    title = "SOFA component that led to Sepsis-3 threshold",
    x = "SOFA component",
    y = "Proportion of sepsis cases",
    colour = "Database"
  ) +
  theme_bw()


ggplot(stats_df$sofa_concomit, aes(index, vals, group = src, colour = src)) + 
  geom_line() + 
  scale_x_discrete(
    limits = str_c("sofa_", c("cardio", "cns", "coag", "liver", "renal", "resp"), "_comp_bef"),
    labels = c("cardio", "cns", "coag", "liver", "renal", "resp")
  ) + 
  labs(
    title = "SOFA component that was elevated before Sepsis-3 threshold",
    x = "SOFA component",
    y = "Proportion of sepsis cases",
    colour = "Database"
  ) +
  theme_bw()


ggplot(stats_df$sofa_adm, aes(as.integer(index), vals, fill = src)) + 
  geom_col(position = "dodge") + 
  labs(
    title = "SOFA score during first hour of ICU",
    x = "SOFA score",
    y = "Proportion of cases",
    fill = "Database"
  ) +
  coord_cartesian(xlim = c(0, 15)) + 
  theme_bw()


ggplot(stats_df$sofa_abs, aes(as.integer(index), vals, fill = src)) + 
  geom_col(position = "dodge") + 
  labs(
    title = "SOFA score at Sepsis-3 Event",
    x = "SOFA score",
    y = "Proportion of cases",
    fill = "Database"
  ) +
  coord_cartesian(xlim = c(0, 15)) + 
  theme_bw()

