







vitals <- c("hr", "o2sat", "temp", "sbp", "map", "dbp", "resp", "etco2") %>% 
  load_concepts(src = src, patient_ids = cohort)

labs <- c("be", "bicar", "fio2", "ph", "pco2", "ast", "bun", "alp", 
          "ca", "cl", "crea", "bili_dir", "glu", "lact", "mg", "phos", 
          "k", "bili", "tri", "hct", "hgb", "ptt", "wbc", "fgn", "plt") %>% 
  load_concepts(src = src)

death <- c("death") %>% 
  load_concepts(src = src)





ricu:::load_concepts.item

