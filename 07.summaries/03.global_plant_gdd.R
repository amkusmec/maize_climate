library(tidyverse)
library(abind)
source("src/bc_sum.R")


hist <- list.files("data/ISIMIP3a/adapted/relative/historical", 
                   "global_bootstrap[0-9]{4}", full.names = TRUE) %>% 
  map(read_rds) %>% 
  abind(along = 4)
ssp <- paste0("ssp", c(126, 370, 585)) %>% 
  map(function(s) {
    cat(s, "\n")
    list.files(paste0("data/ISIMIP3a/adapted/relative/", s), 
               "global_bootstrap[0-9]{4}", full.names = TRUE) %>% 
      map(read_rds) %>% 
      abind(along = 4)
  })


start <- 1983L
end <- 2013L

ref_idx <- which(as.integer(dimnames(hist)[[3]]) %in% start:end)
ref <- apply(hist, c(1, 3, 4), mean, na.rm = TRUE) %>% 
  apply(1, function(x) colMeans(x[ref_idx, ]))

rel <- map(ssp, function(s) {
  apply(s, c(1, 3, 4), mean, na.rm = TRUE) %>%
    apply(2, function(x) exp(x - t(ref)) - 1, simplify = FALSE) %>% 
    abind(along = 3)
})


idx1 <- which(as.integer(dimnames(rel[[1]])[[3]]) %in% 2040:2069)
idx2 <- which(as.integer(dimnames(rel[[1]])[[3]]) %in% 2070:2099)

periods <- map(rel, function(r) {
  map(list(idx1, idx2), function(ii) {
        r[, , ii] %>% 
          apply(1:2, mean, na.rm = TRUE)
      }) %>% 
    abind(along = 3)
})

ensemble <- map(periods, function(p) {
  apply(p, 2:3, mean, na.rm = TRUE)
})


periods_post <- map2_df(periods, paste0("ssp", c(126, 370, 585)),  function(p, s) {
  temp <- apply(p, c(1, 3), bc_sum)
  map_df(1:2, function(i) {
    temp[, , i] %>% 
      t() %>% 
      as_tibble(rownames = "Model") %>% 
      mutate(Scenario = s, 
             Period = if_else(i == 1, "2040-2069", "2070-2099")) %>% 
      select(Scenario, Model, Period, everything())
  })
})

ensemble_post <- map2_df(ensemble, paste0("ssp", c(126, 370, 585)),  function(p, s) {
  apply(p, 2, bc_sum) %>% 
    t() %>% 
    as_tibble() %>% 
    mutate(Scenario = s, 
           Model = "Ensemble", 
           Period = c("2040-2069", "2070-2099")) %>% 
    select(Scenario:Period, everything())
})

write_rds(list(periods, periods_post, ensemble, ensemble_post),
          "data/climate_summaries/plant_gdd.rds")
