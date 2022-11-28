#### -- Packrat Autoloader (version 0.8.1) -- ####
source("packrat/init.R")
#### -- End Packrat Autoloader -- ####

library(magrittr)

# RICU paths and definitions
Sys.setenv(RICU_DATA_PATH = "/Users/patrick/datasets/ricu")

library(ricu)
source("R/callback-circ-fail.R")
source("R/callback-icu-mortality.R")
source("R/callback-kdigo.R")
source("R/callback-sepsis.R")

concept_path <- file.path("config", c("chemistry", "circulatory", "demographics", "hematology", "medications", "misc", "outcomes", "output", "respiratory", "vitals"))
dict <- load_dictionary(cfg_dirs = concept_path)
