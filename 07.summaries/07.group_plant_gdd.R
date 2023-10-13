library(tidyverse)
library(abind)
source("src/bc_sum.R")


hist <- list.files("data/ISIMIP3a/adapted/relative/historical", 
                   "group_bootstrap[0-9]{4}", full.names = TRUE) %>% 
  map(read_rds) %>% 
  abind(along = 5)
ssp <- paste0("ssp", c(126, 370, 585)) %>% 
  map(function(s) {
    cat(s, "\n")
    list.files(paste0("data/ISIMIP3a/adapted/relative/", s), 
               "group_bootstrap[0-9]{4}", full.names = TRUE) %>% 
      map(read_rds) %>% 
      abind(along = 5)
  })


start <- 1983L
end <- 2013L

ref_idx <- which(as.integer(dimnames(hist)[[4]]) %in% start:end)
ref <- apply(hist, c(1:2, 4:5), mean, na.rm = TRUE) %>% 
  apply(1:2, function(x) colMeans(x[ref_idx, ]))
ref <- lapply(1:2000, function(i) ref[i, , ]) %>% 
  abind(along = 3)

rel <- map(ssp, function(s) {
  apply(s, c(1:2, 4:5), mean, na.rm = TRUE) %>%
    apply(3, function(x) exp(x - ref) - 1, simplify = FALSE) %>% 
    abind(along = 4)
})


idx1 <- which(as.integer(dimnames(rel[[1]])[[4]]) %in% 2040:2069)
idx2 <- which(as.integer(dimnames(rel[[1]])[[4]]) %in% 2070:2099)

periods <- map(rel, function(r) {
  map(list(idx1, idx2), function(ii) {
    r[, , , ii] %>% 
      apply(1:3, mean, na.rm = TRUE)
  }) %>% 
    abind(along = 4)
})

ensemble <- map(periods, function(p) {
  apply(p, c(1, 3:4), mean, na.rm = TRUE)
})


periods_post <- map2_df(periods, paste0("ssp", c(126, 370, 585)),  function(p, s) {
  p[p > 0] <- as.numeric(NA)
  temp <- apply(p, c(1:2, 4), bc_sum)
  map_df(1:3, function(i) {
    map_df(1:2, function(j) {
      temp[, i, , j] %>% 
        t() %>% 
        as_tibble(rownames = "Model") %>% 
        mutate(Adaptation = "Planting & maturity", 
               Scenario = s, 
               Period = if_else(j == 1, "2040-2069", "2070-2099"), 
               Group = dimnames(temp)[[2]][i]) %>% 
        select(Adaptation, Group, Scenario, Period, Model, everything())
    })
  })
})

ensemble_post <- map2_df(ensemble, paste0("ssp", c(126, 370, 585)),  function(p, s) {
  p[p > 0] <- as.numeric(NA)
  temp <- apply(p, c(1, 3), bc_sum)
  map_df(1:2, function(i) {
    temp[, , i] %>% 
      t() %>% 
      as_tibble(rownames = "Group") %>% 
      mutate(Adaptation = "Planting & maturity", 
             Scenario = s, 
             Period = if_else(i == 1, "2040-2069", "2070-2099"), 
             Model = "Ensemble") %>% 
      select(Adaptation, Group, Scenario, Period, Model, everything())
  })
})

write_rds(list(periods, periods_post, ensemble, ensemble_post),
          "data/climate_summaries/group_plant_gdd.rds")
