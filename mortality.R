library(argparser)
library(assertthat)
library(rlang)
library(data.table)
library(vctrs)
library(ricu)

source("R/misc.R")
source("R/steps.R")
source("R/sequential.R")
source("R/obs_time.R")


# Create a parser
p <- arg_parser("Extract and preprocess ICU mortality data")
p <- add_argument(p, "--src", help="source database", default="eicu_demo")
argv <- parse_args(p)

src <- argv$src 
conf <- ricu:::read_json("config.json")
path <- file.path(conf$output_dir, "mortality24")


cncpt_env <- new.env()

# Task description
time_flow <- "sequential" # static / sequential / continuous
time_unit <- hours
freq <- 1L
max_len <- hours(24L)

static_vars <- c("age", "sex", "height", "weight")

dynamic_vars <- c("alb", "alp", "alt", "ast", "be", "bicar", "bili", "bili_dir",
                  "bnd", "bun", "ca", "cai", "ck", "ckmb", "cl", "crea", "crp", 
                  "dbp", "fgn", "fio2", "glu", "hgb", "hr", "inr_pt", "k", "lact",
                  "lymph", "map", "mch", "mchc", "mcv", "methb", "mg", "na", "neut", 
                  "o2sat", "pco2", "ph", "phos", "plt", "po2", "ptt", "resp", "sbp", 
                  "temp", "tnt", "urine", "wbc")

# cross-sectional vs longitudinal
predictor_type <- "dynamic" # static / dynamic
outcome_type   <- "static" # static / dynamic



patients <- stay_windows(src, interval = time_unit(freq))
patients <- as_win_tbl(patients, index_var = "start", dur_var = "end", interval = time_unit(freq))



# Define outcome ----------------------------------------------------------

outc <- load_step(dict["death_icu"])



# Define observation times ------------------------------------------------

stop_obs_at(outc, offset = hours(0L), by_ref = TRUE)
stop_obs_at(patients, offset = ricu:::re_time(max_len, time_unit(freq)), by_ref = TRUE)



# Apply exclusion criteria ------------------------------------------------

# 1. Age <18
x <- load_step(dict["age"])
x <- filter_step(x, ~ . < 18)

excl1 <- unique(x[, id_vars(x), with = FALSE])


# 2. Died within the first 30 hours of ICU admission
x <- filter_step(outc, ~ . < 30, col = index_col)

excl2 <- unique(x[, id_vars(x), with = FALSE])


# 3. LoS less than 24 hours
x <- load_step(dict["los_icu"])
x <- filter_step(x, ~ . < 1)

excl3 <- unique(x[, id_vars(x), with = FALSE])


# 4. observation periods without any measured dynamic predictors
inverse_step <- function(x) {
  id <- id_vars(x)
  patients[!x, on = c(id), .SD, .SDcols = id]
}

x <- load_step(dict[dynamic_vars], interval = time_unit(freq), cache = TRUE)
x <- filter_step(x, patients)
x <- inverse_step(x)

excl4 <- unique(x[, id_vars(x), with = FALSE])


# Apply exclusions
patients <- patients[!excl1][!excl2][!excl3][!excl4]
patient_ids <- patients[, .SD, .SDcols = id_var(patients)]


# Prepare data ------------------------------------------------------------

# Get predictors
dyn <- load_step(dict[dynamic_vars], cache = TRUE)
sta <- load_step(dict[static_vars], cache = TRUE)

# Transform all variables into the target format
assert_that(outcome_type == "static", time_flow == "sequential")

map_to_patients <- function(x) {
  grid <- patients[, .SD, .SDcols = id_var(patients)]
  merge(grid, x, all.x = TRUE)
}

outc_fmt <- function_step(outc, map_to_patients)
outc_fmt <- mutate_step(outc_fmt, ricu::replace_na, val = FALSE)
outc_fmt <- mutate_step(outc_fmt, as.integer)
# TODO: make step to add/remove columns
ind <- index_var(outc_fmt)
outc_fmt[, c(ind) := NULL]
rename_cols(outc_fmt, c("stay_id", "label"), by_ref = TRUE)

dyn_fmt <- function_step(dyn, map_to_grid)
dyn_fmt <- filter_step(dyn_fmt, patients)
rename_cols(dyn_fmt, c("stay_id", "time"), meta_vars(dyn_fmt), by_ref = TRUE)

sta_fmt <- function_step(sta, map_to_patients)
rename_cols(sta_fmt, c("stay_id"), id_vars(sta), by_ref = TRUE)



# Write to disk -----------------------------------------------------------

out_path <- paste0(path, "/", src)

if (!dir.exists(out_path)) {
  dir.create(out_path, recursive = TRUE)
}

arrow::write_parquet(outc_fmt, paste0(out_path, "/outc.parquet"))
arrow::write_parquet(dyn_fmt, paste0(out_path, "/dyn.parquet"))
arrow::write_parquet(sta_fmt, paste0(out_path, "/sta.parquet"))
