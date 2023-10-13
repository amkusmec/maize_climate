library(tidyverse)

# Rate of increase = 0.65% per yr (Zhu et al. 2019)
gdd_rate <- 0.0065

# GCM models
models <- list.dirs("data/ISIMIP3a/adapted/distributions", 
                    recursive = FALSE, full.names = FALSE)

for (m in models) {
  paste0("data/ISIMIP3a/adapted/distributions/", 
         m, "/estimated_season_lengths.csv") %>% 
    read_csv(col_types = cols()) %>% 
    group_by(State, County) %>% 
    summarise(GDD = mean(GDD, na.rm = TRUE), 
              .groups = "drop") %>% 
    rowwise() %>% 
    mutate(GDDnew = list({ 
        v <- tibble(Year = 2015:2100, 
                    GDDnew = 0)
        for (i in 1:nrow(v)) {
          if (i == 1) v$GDDnew[i] <- GDD[1]*(1 + gdd_rate)^2
          else v$GDDnew[i] <- v$GDDnew[i - 1]*(1 + gdd_rate)
        }
        
        v
      })) %>% 
    ungroup() %>% 
    unnest(GDDnew) %>% 
    select(-GDD) %>% 
    write_csv(paste0("data/ISIMIP3a/adapted/distributions/", 
                     m, "/adapted_gdds.csv"))
}
