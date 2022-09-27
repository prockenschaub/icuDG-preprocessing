library(assertthat)
library(rlang)
library(data.table)
library(vctrs)
library(ricu)

source("R/steps.R")
source("R/sequential.R")
source("R/obs_time.R")

src <- "hirid"


cncpt_env <- new.env()

# Task description
time_flow <- "sequential" # sequential / continuous
time_unit <- mins
freq <- 5L
max_len <- 7 * 24 * 60  # = 7 days

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



# 1. Age <16 or >100 years
x <- load_step("age")
x <- filter_step(x, ~ . < 16 | . > 100)

excl1 <- unique(x[, id_vars(x), with = FALSE])


# 2. admissions with only missing circulatory status 
inverse_step <- function(x) {
  id <- id_vars(x)
  patients[!x, on = c(id), .SD, .SDcols = id]
}

x <- load_step("circ_fail", interval = time_unit(freq), cache = TRUE)
x <- mutate_step(x, ~ !is.na(.))
x <- summary_step(x, "any")
x <- filter_step(x, ~ . == TRUE)
x <- inverse_step(x)

excl2 <- unique(x[, id_vars(x), with = FALSE])


# 3. mechanical circulatory support
x <- load_step("mech_circ")

excl3 <- unique(x[, id_vars(x), with = FALSE])



# Apply exclusions
patients <- patients[!excl1][!excl2][!excl3]
patient_ids <- patients[, .SD, .SDcols = id_var(patients)]



# Get outcome and predictors
outc <- load_step("circ_fail", interval = time_unit(freq), cache = TRUE)
dyn <- load_step(dynamic_vars, interval = time_unit(freq), cache = TRUE)
sta <- load_step(static_vars, cache = TRUE)



# Restrict observation times
stop_obs_at(patients, offset = mins(max_len), by_ref = TRUE)


# Transform all variables into the target format
assert_that(outcome_type == "dynamic", time_flow == "sequential")

outc_fmt <- function_step(outc, map_to_grid)
rename_cols(outc_fmt, c("stay_id", "time", "label"), by_ref = TRUE)

dyn_fmt <- function_step(dyn, map_to_grid)
rename_cols(dyn_fmt, c("stay_id", "time"), meta_vars(dyn_fmt), by_ref = TRUE)

sta_fmt <- sta[patient_ids]  # TODO: make into step
rename_cols(sta_fmt, c("stay_id"), id_vars(sta), by_ref = TRUE)

fwrite(outc_fmt, "/Users/patrick/datasets/benchmark/circ_fail/mimic/outc.csv.gzip", compress = "gzip")
fwrite(dyn_fmt, "/Users/patrick/datasets/benchmark/circ_fail/mimic/dyn.csv", compress = "gzip")
fwrite(sta_fmt, "/Users/patrick/datasets/benchmark/circ_fail/mimic/sta.csv", compress = "gzip")

