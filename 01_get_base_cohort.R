
library(tidyverse)
library(ricu)
library(glue)

dict <- load_dictionary(src)

# Define super-cohort as adult patients with sex information
unq_ids <- rbind_lst(list(
  load_id("icustays", "miiv", cols = NULL),
  load_id("patient", "eicu", cols = NULL),
  load_id("general", "hirid", cols = NULL)
))

print(glue("Total number of patients: {nrow(unq_ids)}"))

cohort <- load_concepts(c("age", "sex"), src, verbose=FALSE) %>% 
  filter(
    !is.na(age), 
    !is.na(sex)
  )

print(glue(
  "Patients with demographic info: {nrow(cohort)} / {nrow(unq_ids)} ",
  "({round(nrow(cohort) / nrow(unq_ids) * 100, 2)}%)"
))

cohort <- cohort[age >= 18 & age < 90, ]

print(glue(
  "Patients aged 18-89: {nrow(cohort)} / {nrow(unq_ids)} ",
  "({round(nrow(cohort) / nrow(unq_ids) * 100, 2)}%)"
))


# Add hospital information (mainly for eICU)
hosp_ids <- load_concepts("hospital_id", src = src, patient_ids = cohort)
cohort <- inner_join(
  cohort, 
  hosp_ids, 
  by = c("source", "stay_id")
)

# Add total length of stay
los <- load_concepts(c("los_hosp", "los_icu"), src = src)
cohort <- inner_join(
  cohort, 
  los, 
  by = c("source", "stay_id")
)


load_concepts(c("los_icu"), src = "hirid")


cohort %>% 
  select(-contains("_id")) %>% 
  group_by(source) %>% 
  skim()

psych::describe(cohort %>% group)

ricu:::los_callback
ricu:::setup_src_env.src_cfg

load_concepts(c("time_to_icu_adm", "time_to_icu_disch"), src = "eicu", interval = mins(1))




sers <- load_difftime(hirid$observations)


