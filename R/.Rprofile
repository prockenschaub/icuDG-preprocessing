#source("renv/activate.R")

Sys.setenv(RICU_DATA_PATH = "/Users/patrick/datasets/ricu")
if(require("ricu", quietly = TRUE)) {
  source("../ricu-extensions/callbacks/callback-icu-mortality.R")
  source("../ricu-extensions/callbacks/callback-kdigo.R")
  source("../ricu-extensions/callbacks/callback-sepsis.R")
  
  concept_path <- file.path("..", "ricu-extensions", "configs", c("chemistry", "circulatory", "demographics", "hematology", "medications", "misc", "outcomes", "output", "respiratory", "vitals"))
  dict <- load_dictionary(cfg_dirs = concept_path)
}
