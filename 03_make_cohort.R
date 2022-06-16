
library(tidyverse)
library(ricu)
library(glue)
library(magrittr)


src <- "mimic"

attrition <- tibble( # <- store the patient attrition for inspection
  desc = vector("character", 0L),
  n = vector("integer", 0L)
)

log_result <- function(desc, n) {
  attrition %<>% bind_rows(list(desc = desc, n = n))
  assign("attrition", attrition, envir = globalenv())
}


# Load derived datasets
id <- as_id_cfg(src)

static <- read_rds(glue("derived/{src}/static.rds"))
vitals <- read_rds(glue("derived/{src}/vitals.rds"))
labs <- read_rds(glue("derived/{src}/labs.rds"))
aux <- read_rds(glue("derived/{src}/aux.rds"))
sep3 <- read_rds(glue("derived/{src}/sep3.rds"))


# Get all patients in the database ---------------------------------------------
cohort <- stay_windows(src, interval = mins(1L))
attrition %<>% bind_rows(list(desc = "all", n = nrow(cohort)))

`%exclude%` <- function(from, df) {
  anti_join(from, df, by = id_var(from))
}

`%remain_in%` <- function(df, within) {
  semi_join(df, within, by = id_var(within))
}


# Apply exclusions -------------------------------------------------------------

# Exclude children
children <- static$age %>% 
  filter(age < 1)

cohort <- cohort %exclude% children
log_result("age < 14 years", nrow(children))


# Exclude patients with sepsis onset before coming to the ICU
sepsis_before_icu <- (sep3 %remain_in% cohort)  %>% 
  filter(charttime < 0)

cohort <- cohort %exclude% sepsis_before_icu
log_result("onset not during ICU stay", nrow(sepsis_before_icu))


# Exclude patients with sepsis less than 4h or more than 168h after ICU adm
sepsis_outside_window <- (sep3 %remain_in% cohort) %>% 
  filter(charttime < 4 | charttime > 168)

cohort <- cohort %exclude% sepsis_outside_window
log_result("onset no in [4h, 168h]", nrow(sepsis_outside_window))


# Exclude ICU stays that are shorter than 6h
short_stay <- (aux$los_icu %remain_in% cohort) %>% 
  filter(los_icu < 6/24) # concept is in days

cohort <- cohort %exclude% short_stay
log_result("stay length < 6h", nrow(short_stay))


# Exclude patients with <4 in ICU measurements
#
# NOTE: it is not clearly defined in the paper what "measurement" relates to, 
#       e.g., if both sbp and dbp are measured simultaneously, is this 1 or 2
#       measurements? We go with the former interpretation. 
icu_measurements <- c(vitals, labs) %>% 
  map(select, 1:2) %>% 
  rbind_lst() %>% 
  distinct() %>% 
  inner_join(cohort,by = id_vars(cohort)) %>% 
  filter(  # Remove measurements post-ICU
    start <= charttime, charttime < end
  ) %>% 
  select(1:2)

few_measurements <- icu_measurements %>% 
  count(icustay_id) %>% 
  filter(n < 4)

cohort <- cohort %exclude% few_measurements
log_result("< 4 in ICU measurements", nrow(few_measurements))


# Exclude patients with missing data windows >12h
missing_data <- icu_measurements %>% 
  set_names(c("id", "time")) %>% 
  mutate(
    diff = ifelse(id == lag(id), time - lag(time), NA),
    diff = diff / 60  # convert to hours
  ) %>% 
  filter(!is.na(diff)) %>% 
  group_by(id) %>% 
  summarise(max_diff = max(diff, na.rm = TRUE), .groups = "drop") %>% 
  filter(max_diff > 12) %>% 
  select(id) %>% 
  set_names(id_vars(icu_measurements))  # TODO: this currently does not account for gaps before the first or after the last measurement

cohort <- cohort %exclude% missing_data
log_result("missing data window > 12h", nrow(missing_data))



# Tally the attrition
attrition <- attrition %>% 
  mutate(n_cohort = n[1] - cumsum(c(0, n[-1])))


write_rds(cohort, glue("derived/{src}/cohort.rds"))
write_rds(attrition, glue("derived/{src}/attrition.rds"))

