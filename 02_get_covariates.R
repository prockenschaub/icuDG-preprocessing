

library(tidyverse)
library(ricu)
library(glue)
library(testthat)

dict <- load_dictionary(src)
src <- "mimic"


# ------------------------------------------------------------------------------
# Vital signs
#
# heart rate (hr): no issues.
# oxygen saturation (o2sat): 6% missing, which is mostly due to eicu where 
#   this variable is stored in wide format (together with the more frequently
#   recorded variable heart rate).
# temperature (temp): 86% missing, due to miiv and eicu. For miiv, an incorrect 
#   itemid is included in the concept definition, which is entirely missing. For
#   eicu, there is the same issue as with o2sat.
# systolic BP (sbp): 57% missing, due to eicu (see o2sat).
# mean arterial pressure (map): 52% missing, due to eicu (see o2sat).
# diastolic BP (dbp): 57% missing, due to eicu (see o2sat).
# respiratory rate (resp): 9% missing, due to eicu (see o2sat).
# end-tidal CO2 (etco2): 1% missing, not currently 


vitals <- c("hr", "o2sat", "temp", "sbp", "map", "dbp", "resp", "etco2") %>% 
  load_concepts(
    src = src, 
    merge = FALSE, 
    interval = mins(1L), 
    aggregate = FALSE
  )

write_rds(vitals, glue("derived/{src}/vitals.rds"))



# ------------------------------------------------------------------------------
# Laboratory tests
#
# * Base excess (be): no issues.
# * Bicarbonate (bicar): no issues.
# * Fraction of inspired O2 (fio2): no major issues (very infrequent Lpm)
# * pH (ph): no major issues (unit naming convention varies)
# * Partial pressure of CO2 (pco2): no issues.
# * Aspartate Transferase (ast): no major issues (very infrequent units/mL)
# * Blood urea nitrogen (bun): no issues.
# * Alkaline phosphatase (alp): no issues.
# * Calcium (ca): no issues.
# * Chloride (cl): no issues.
# * Creatinine (crea): no issues.
# x Direct bilirubin (bili_dir): ricu does not consider threshold records (e.g., 
#     <0.2, <0.1, etc.). Replace these with their threshold value.
# * Glucose (glu): no issues.
# * Lactate (lact): no issues.
# * Magnesium (mg): no issues.
# * Phosphate (phos): no issues.
# * Potassium (k): no issues.
# * Total bilirubin (bili): no issues.
# x Troponin I (tri): only available in eICU, where there are thresholding issues
#     similar to those seen for direct bilirubin.
# x Troponin T (tnt): thresholding issues similar to those seen for direct 
#     bilirubin.
# * Haematocrit (hct): no major issues (very infrequent %PCV).
# * Haemoglobin (hgb): no issues.
# x Protrombin time (ptt): thresholding issue but slightly more complicated than 
#     for direct bilirubin (frequently >)
# White blood cells (wbc): no major issues (unit naming convention varies 
#   frequently).
# Fibrinogen (fgn): no issues.
# Platelets (plt): no majori issues (unit naming convention varies 
#   frequently).
# INR (inr_pt): thresholding issue, particularly in HiRID


labs <- c("alp", "ast", "be", "bicar", "bili", "bili_dir", "bun", "ca", "cl", 
          "crea", "fgn", "fio2", "glu", "hct", "hgb", "inr_pt", "k", "lact", 
          "mg", "pco2", "ph", "phos", "plt", "ptt", "tri", "tnt",  "wbc"
        ) %>% 
  load_concepts(
    src = src, 
    merge = FALSE, 
    interval = mins(1L), 
    aggregate = FALSE
  )

write_rds(labs, glue("derived/{src}/labs.rds"))
