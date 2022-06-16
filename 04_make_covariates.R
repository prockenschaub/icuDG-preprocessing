library(tidyverse)
library(ricu)
library(glue)
library(magrittr)


src <- "mimic"


# Load derived datasets
vitals <- read_rds(glue("derived/{src}/vitals.rds"))
labs <- read_rds(glue("derived/{src}/labs.rds"))

measures <- c(vitals, labs)


# Limit them to measurements made within the ICU -------------------------------
stays <- stay_windows(src)

#' Select those measurements that fall within ICU start and end date
select_in_icu <- function(measurements, stays) {
  inner_join(measurements, stays, by = id_vars(stays)) %>% 
    filter(
      start <= get(time_vars(measurements)),
      get(time_vars(measurements)) < end
    ) %>% 
    select(-start, -end)
}

measures <- map(measures, select_in_icu, stays = stays)



# Create aggregates ------------------------------------------------------------

#' Change interval and aggregate via default function
#' @seealso [ricu:::aggregate.id_tbl()]
aggregate_covariates <- function(df, interval = hours(1L)) {
  df %>% 
    change_interval(interval) %>% 
    aggregate()
}


agg <- map(measures, aggregate_covariates)
  


# Create missingness indicators ------------------------------------------------

#' Make a time grid of `interval` steps for each patients ICU stay up to an 
#' upper limit of `max_time` steps.
#' 
#' @return `ts_tbl` with src `icustay` identifier and `time` column
time_grid <- function(stays, interval = hours(1L), max_time = hours(192L)) {
  stays %>% 
    change_interval(interval) %>% 
    pivot_longer(
      cols = all_of(c("start", "end")),
      names_to = NULL, 
      values_to = "time"
    ) %>% 
    mutate(time = pmin(time, max_time)) %>% 
    as_ts_tbl(
      id_vars = id_vars(stays), 
      index_var = "time"
    ) %>% 
    unique() %>% 
    fill_gaps()
}

grid <- time_grid(stays)

#' Use the time grid to explicitly list missing values and then turn them into
#' an indicator matrix
mask_missing <- function(df, grid) {
  left_join(
      grid, 
      df %>% change_interval(interval(grid)),
      by = c(id_vars(stays), time = index_var(df))
    ) %>% 
    rename(!!index_var(df) := time) %>% 
    mutate(!!data_var(df) := as.integer(is.na(.data[[data_var(df)]])))
}

miss <- agg %>% 
  map(mask_missing, grid)



# Create measurement counts ----------------------------------------------------

count_measurements <- function(df, interval = hours(1L)) {
  df %>% 
    change_interval(interval) %>% 
    mutate(!!data_var(df) := 1) %>% 
    aggregate(expr = "sum")
}

cnts <- map(measures, count_measurements)




# Bring it all together --------------------------------------------------------

add_suffix <- function(df, suffix) {
  newnames <- names(df)
  idx <- which(newnames %in% data_vars(df))
  newnames[idx] <- str_c(newnames[idx], "_", suffix)
  rename_cols(df, newnames)
}


predictors <- c(
    agg, 
    map(miss, add_suffix, "miss"), 
    map(cnts, add_suffix, "cnt")
  ) %>% 
  merge_lst()


