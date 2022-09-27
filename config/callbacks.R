
source("config/callback-circ-fail.R")


# ------------------------------------------------------------------------------
# Admission time functions
time_to_callback <- function (x, id_type, interval) 
{
  to <- x[["to"]]
  cfg <- as_id_cfg(x)
  res <- id_map(x, id_vars(cfg["hadm"]), id_vars(cfg["icustay"]), "start", "end")
  
  res <- ricu:::set_id_vars(res, id_vars(cfg["icustay"]))
  
  if (to == "icu_adm") {
    res[, `:=`(c("val_var", "start", "end"), list(get("start"), NULL, NULL))]
  } else if (to == "icu_disch") {
    res[, `:=`(c("val_var", "start", "end"), list(get("end"), NULL, NULL))]
  }
  res[, `:=`("val_var", list(get("val_var")))]
  res
}



# ------------------------------------------------------------------------------
# MIMIC IV specific functions

miiv_death <- function(x, val_var, sub_var, env, ...) {
  
}


