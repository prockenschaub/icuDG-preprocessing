source("renv/activate.R")

library(magrittr)

# RICU paths and definitions

Sys.setenv(RICU_DATA_PATH = "/Users/patrick/datasets/ricu") # NOTE: change this your own ricu data path ?ricu::import_src

library(ricu)
source("R/callback-circ-fail.R")
source("R/callback-icu-mortality.R")
source("R/callback-kdigo.R")
source("R/callback-sepsis.R")

concept_path <- file.path("config", c("chemistry", "circulatory", "demographics", "hematology", "medications", "misc", "outcomes", "output", "respiratory", "vitals"))
dict <- load_dictionary(cfg_dirs = concept_path)
