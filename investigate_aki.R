library(tidyverse)

conf <- ricu:::read_json("config.json")
base <- arrow::read_parquet(file.path(conf$output_dir, "aki", "miiv", "sta.parquet"))


sers <- load_concepts(dict['kdigo'], "miiv", keep_components = TRUE)
sersu <- load_concepts(dict['kdigo_urine'], "miiv", keep_components = TRUE)

ever <- sers[, .(crea = any(ricu:::is_true(kdigo_crea > 1)), urine = any(ricu:::is_true(kdigo_urine > 1))), by = "stay_id"]
in_all <- merge(base, ever, all.x = TRUE)
in_all <- in_all[, .(crea = ricu:::replace_na(crea, FALSE), urine = ricu:::replace_na(urine, FALSE))]

heisl <- load_concepts(dict['kdigo'], "aumc", keep_components = TRUE)


src <- "hirid"

age <- load_concepts(dict['age'], src)
adults <- age[age >= 18]

crea <- load_concepts(dict['crea'], src)
crea_few <- crea[datetime >= 0][, .N, by = patientid][N <= 1]
crea_high_base <- crea[, .(high_base = (datetime < 0 | seq_len(.N) == 1) & crea > 5), by = patientid][high_base == TRUE]

urine <- load_concepts(dict['urine'], src)
urine_few <- urine[datetime >= 0][, .N, by = patientid][N <= 1]

cohort <- adults[!crea_few][!crea_high_base][!urine_few]


kdigo_u <- load_concepts(dict['kdigo_urine'], src, keep_components = TRUE, patient_ids = cohort$patientid)
kdigo_c <- load_concepts(dict['kdigo_crea'], src, keep_components = TRUE, patient_ids = cohort$patientid)
kdigo_a <- load_concepts(dict['kdigo'], src, keep_components = TRUE, patient_ids = cohort$patientid)



mim <- load_concepts(dict['kdigo'], 'mimic', keep_components = TRUE)
eic <- load_concepts(dict['kdigo_crea'], 'eicu')
hir <- load_concepts(dict['kdigo'], 'hirid', keep_components = TRUE)
aum <- load_concepts(dict['kdigo'], 'aumc', keep_components = TRUE)

prev <- function(dt){
  mean(dt[, .(crea = max(kdigo_crea)), by = c(id_var(dt))]$crea > 0)
}



out <- hirid$observations[hirid$observations$variableid==30005110, ]
vol <- hirid$observations[hirid$observations$variableid==10020000, ]



urine_rt_itm <- new_itm("hirid", table = "observations", sub_var = "variableid", ids = 10020000, class = "hrd_itm", target = "ts_tbl")
urine_rt <- new_cncpt("urine_rt", list(urine_rt_itm), aggregate = "mean")

urine <- load_concepts(urine_rt, src ="hirid")
urine_6h <- slide(urine, list(urine_rt_6hr = max(urine_rt)), hours(6L), left_closed = FALSE)
urine_12h <- slide(urine, list(urine_rt_12hr = max(urine_rt)), hours(12L), left_closed = FALSE)
urine_24h <- slide(urine, list(urine_rt_24hr = max(urine_rt)), hours(24L), left_closed = FALSE)

weight <- load_concepts(dict['weight'], src ="hirid")

kdigo_urine <- merge_lst(list(urine_6h, urine_12h, urine_24h, weight))
kdigo_urine[,
  c("urine_rt_6hr", "urine_rt_12hr", "urine_rt_24hr") := 
    .(urine_rt_6hr / weight, urine_rt_12hr / weight, urine_rt_24hr / weight)
]

kdigo_urine[, kdigo_urine := data.table::fcase(
  datetime >= 24 & urine_rt_24hr < 0.3, 3L,
  datetime >= 12 & urine_rt_12hr == 0 , 3L,
  datetime >= 12 & urine_rt_12hr < 0.5, 2L,
  datetime >=  6 & urine_rt_6hr  < 0.5, 1L,
  default = 0L
)]


load_kdigo_urine <- function(src) {
  urine_rate <- load_concepts(dict['urine_rate'], src = src)
  weight <- load_concepts(dict['weight'], src = src)
  md_ku <- load_concepts(dict['kdigo_urine'], src = src, keep_components = TRUE)
  md_ku2 <- kdigo_urine2(urine_rate = urine_rate, weight = weight, keep_components = TRUE)
  
  list(old = md_ku, new = md_ku2)
}

aumc <- load_kdigo_urine("aumc")
aumc$old[, .(tri = max(kdigo_urine)), by = admissionid]$tri %>% table() %>% prop.table()
aumc$new[, .(tri = max(kdigo_urine)), by = admissionid]$tri %>% table() %>% prop.table()

hir <- load_kdigo_urine("hirid")
hir$old[, .(tri = max(kdigo_urine)), by = patientid]$tri %>% table() %>% prop.table()
hir$new[, .(tri = max(kdigo_urine)), by = patientid]$tri %>% table() %>% prop.table()

mim <- load_kdigo_urine("mimic")
mim$old[, .(tri = max(kdigo_urine)), by = icustay_id]$tri %>% table() %>% prop.table()
mim$new[, .(tri = max(kdigo_urine)), by = icustay_id]$tri %>% table() %>% prop.table()

mii <- load_kdigo_urine("miiv")
mii$old[, .(tri = max(kdigo_urine)), by = stay_id]$tri %>% table() %>% prop.table()
mii$new[, .(tri = max(kdigo_urine)), by = stay_id]$tri %>% table() %>% prop.table()

eic <- load_kdigo_urine("eicu")
eic$old[, .(tri = max(kdigo_urine)), by = patientunitstayid]$tri %>% table() %>% prop.table()
eic$new[, .(tri = max(kdigo_urine)), by = patientunitstayid]$tri %>% table() %>% prop.table()

eur <- load_concepts(dict['urine_rate'], src = "eicu")
ewe <- load_concepts(dict['weight'], src = "eicu")
eic$new <- kdigo_urine2(urine_rate = eur, weight = ewe, keep_components = TRUE)
