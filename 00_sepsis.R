
library(tidyverse)
library(ricu)
library(glue)
library(testthat)


src <- "mimic"
dict <- load_dictionary(src)


load_from_src <- partial(load_concepts, src = src, verbose = TRUE)


# Calculate SOFA score ---------------------------------------------------------

# -> cardio
map <- load_from_src("map", aggregate = "min")
dopa60 <- load_from_src("dopa60", aggregate = "max")
norepi60 <- load_from_src("norepi60", aggregate = "max")
dobu60 <- load_from_src("dobu60", aggregate = "max")
epi60 <- load_from_src("epi60", aggregate = "max")

sofa_cardio <- ricu::sofa_cardio(
  mget(c("map", "dopa60", "norepi60", "dobu60", "epi60"))
)

# -> cns
gcs <- load_from_src("gcs")
sofa_cns <- ricu::sofa_cns(data.table::copy(gcs))

# -> coagulation
plt <- load_from_src("plt", aggregate = "min")
sofa_coag <- ricu::sofa_coag(data.table::copy(plt))

# -> liver
bili <- load_from_src("bili", aggregate = "max")
sofa_liver <- ricu::sofa_liver(data.table::copy(bili))

# -> kidney
crea <- load_from_src("crea", aggregate = "max")
urine24 <- load_from_src("urine24")
sofa_renal <- ricu::sofa_renal(list(crea = crea, urine24 = urine24))

# -> respiratory
pafi <- load_from_src("pafi")
vent_ind <- load_from_src("vent_ind")
sofa_resp <- ricu::sofa_resp(list(pafi = pafi, vent_ind = vent_ind))

# => Total score
sofa <- sofa_score(
  mget(c(
    "sofa_resp", "sofa_coag", "sofa_liver", 
    "sofa_cardio", "sofa_cns", "sofa_renal"
)))



# Define suspicion of infection ------------------------------------------------

# Antibiotic administration
abx <- load_from_src("abx")
samp <- load_from_src("samp")

# NOTE: 
# not all data sources have reliable information on blood sampling. Following 
# Moor et al. (2021), use Abx + blood sampling where available to define
# suspicion of infection and use multiple Abx otherwise. 
if (src %in% c("miiv", "mimic", "aumc")) {
  susp_inf <- ricu::susp_inf(list(abx = abx, samp = samp))
} else {
  susp_inf <- ricu::susp_inf(
    list(abx = abx, samp = samp),
    si_mode = "abx",
    abx_min_count = 2,
    abx_count_win = hours(24L),
    by_ref = FALSE
  )
}



# Use suspicion of infection and SOFA to define Sepsis 3 -----------------------

sep3 <- ricu::sep3(list(sofa = sofa, susp_inf = susp_inf))




# Save everything --------------------------------------------------------------

mget(ls(pattern = "sofa")) %>% 
  map2(., names(.), ~ write_rds(.x, glue("derived/{src}/{.y}.rds")))
write_rds(abx, glue("derived/{src}/abx.rds"))
write_rds(susp_inf, glue("derived/{src}/susp_inf.rds"))
write_rds(sep3, glue("derived/{src}/sep3.rds"))


