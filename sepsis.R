library(assertthat)
library(rlang)
library(data.table)
library(vctrs)
library(ricu)

source("R/steps.R")
source("R/sequential.R")
source("R/obs_time.R")

src <- "mimic"


cncpt_env <- new.env()

# Task description
time_flow <- "sequential" # sequential / continuous
time_unit <- hours
freq <- 1L
max_len <- 7 * 24  # = 7 days

static_vars <- c("age", "sex", "height", "weight")

dynamic_vars <- c("alb", "alp", "alt", "ast", "be", "bicar", "bili", "bili_dir",
          "bnd", "bun", "ca", "cai", "ck", "ckmb", "cl", "crea", "crp", 
          "dbp", "fgn", "fio2", "glu", "hgb", "hr", "inr_pt", "k", "lact",
          "lymph", "map", "mch", "mchc", "mcv", "methb", "mg", "na", "neut", 
          "o2sat", "pco2", "ph", "phos", "plt", "po2", "ptt", "resp", "sbp", 
          "temp", "tnt", "urine", "wbc")


# cross-sectional vs longitudinal
predictor_type <- "dynamic" # static / dynamic
outcome_type   <- "dynamic" # static / dynamic


patients <- stay_windows(src, interval = time_unit(freq))
patients <- as_ts_tbl(patients, index_var = "start", interval = time_unit(freq))
patients[, end := pmin(end, time_unit(max_len))]



# Exclusions --------------------------------------------------------------

# 1. Age < 18
x <- load_step("age")
x <- filter_step(x, ~ . < 18)

excl1 <- unique(x[, id_vars(x), with = FALSE])

# Low sepsis prevalence
prevalence <- function(concept, hospital_ids, ...) {
  assert_that(is_logical(data_col(concept)))
  var <- data_var(concept)
  cncpt_per_hosp <- concept[hospital_ids]
  cncpt_per_hosp[, (var) := ricu::replace_na(.SD[[var]], FALSE)]
  prevalence <- cncpt_per_hosp[, .(prev = mean(.SD[[var]])), by = hospital_id]
  res <- merge(hospital_ids, prevalence, by = "hospital_id")
  rm_cols(res, "hospital_id")
}

x1 <- load_step("sofa", cache = TRUE)
#x2 <- load_step("susp_inf", si_mode = "abx", abx_min_count = 2, abx_count_win = hours(24L))
x2 <- load_step("susp_inf", cache = TRUE)
x3 <- function_step(list(x1, x2), sep3)


x4 <- summary_step(x3, "exists")
x5 <- load_step("hospital_id")
x6 <- function_step(x4, prevalence, hospital_ids = x5)
x7 <- filter_step(x6, ~ . < 0.15)

excl2 <- unique(x7[, id_vars(x), with = FALSE])

# Sepsis onset before ICU
x1 <- load_step("sofa")
#x2 <- load_step("susp_inf", si_mode = "abx", abx_min_count = 2, abx_count_win = hours(24L))
x2 <- load_step("susp_inf")
x3 <- function_step(list(x1, x2), sep3)
x4 <- summary_step(x3, "first")
x5 <- filter_step(x4, ~ . < 0, col = index_col)

excl3 <- unique(x5[, id_vars(x), with = FALSE])

# Sepsis onset outside of [4h, 168h]
x1 <- load_step("sofa")
#x2 <- load_step("susp_inf", si_mode = "abx", abx_min_count = 2, abx_count_win = hours(24L))
x2 <- load_step("susp_inf")
x3 <- function_step(list(x1, x2), sep3)
x4 <- summary_step(x3, "first")
x5 <- filter_step(x4, ~ . < 4 | . > 168, col = index_col)

excl4 <- unique(x5[, id_vars(x), with = FALSE])

# Stay <6h
x <- load_step("los_icu")
x <- filter_step(x, ~ . < 6 / 24)

excl5 <- unique(x[, id_vars(x), with = FALSE])

# Less than 4 measurements
n_obs_per_row <- function(x, ...) {
  # TODO: make sure this does not change by reference if a single concept is provided
  obs <- data_vars(x)
  x[, n := as.vector(rowSums(!is.na(.SD))), .SDcols = obs]
  x[, .SD, .SDcols = !c(obs)]
}

x <- load_step(dynamic_vars, cache = TRUE)
x <- summary_step(x, "count", drop_index = TRUE)
x <- filter_step(x, ~ . < 4)

excl6 <- unique(x[, id_vars(x), with = FALSE])


# More than 12 hour gaps between measurements
map_to_grid <- function(x) {
  grid <- expand(patients)
  merge(grid, x, all.x = TRUE)
}

longest_rle <- function(x, val) {
  x <- x[, rle(.SD[[data_var(x)]]), by = c(id_vars(x))]
  x <- x[values != val, lengths := 0]
  x[, .(lengths = max(lengths)), , by = c(id_vars(x))]
}

x <- load_step(dynamic_vars, cache = TRUE)
x <- function_step(x, map_to_grid)
x <- function_step(x, n_obs_per_row)
x <- mutate_step(x, ~ . > 0)
x <- function_step(x, longest_rle, val = FALSE)
x <- filter_step(x, ~ . >= 12)

excl7 <- unique(x[, id_vars(x), with = FALSE])



# Apply exclusions
patients <- patients[!excl1][!excl2][!excl3][!excl4][!excl5][!excl6][!excl7]
patient_ids <- patients[, .SD, .SDcols = id_var(patients)]


# Get outcome and predictors
x1 <- load_step("sofa", cache = TRUE)
#x2 <- load_step("susp_inf", si_mode = "abx", abx_min_count = 2, abx_count_win = hours(24L))
x2 <- load_step("susp_inf", cache = TRUE)
x3 <- function_step(list(x1, x2), sep3)

outc <- summary_step(x3, "first")
dyn <- load_step(dynamic_vars, cache = TRUE)
sta <- load_step(static_vars, cache = TRUE)


# Restrict observation times
stop_obs_at(outc, offset = hours(24L), by_ref = TRUE)
stop_obs_at(patients, offset = hours(max_len + 24L), by_ref = TRUE)


# Transform all variables into the target format
assert_that(outcome_type == "dynamic", time_flow == "sequential")

outc_fmt <- function_step(outc, map_to_grid)
outc_fmt <- function_step(outc_fmt, outcome_window, window = c(6L, 24L))
rename_cols(outc_fmt, c("stay_id", "time", "label"), by_ref = TRUE)

dyn_fmt <- function_step(dyn, map_to_grid)
rename_cols(dyn_fmt, c("stay_id", "time"), meta_vars(dyn_fmt), by_ref = TRUE)

sta_fmt <- sta[patient_ids]  # TODO: make into step
rename_cols(sta_fmt, c("stay_id"), id_vars(sta), by_ref = TRUE)

fwrite(outc_fmt, "/Users/patrick/datasets/benchmark/sepsis/mimic/outc.csv.gz")
fwrite(dyn_fmt, "/Users/patrick/datasets/benchmark/sepsis/mimic/dyn.csv.gz")
fwrite(sta_fmt, "/Users/patrick/datasets/benchmark/sepsis/mimic/sta.csv.gz")

