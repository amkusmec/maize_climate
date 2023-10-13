library(tidyverse)
library(lubridate)
library(parallel)


options(readr.show_progress = FALSE)

### Link PRISM grid cells to the counties where trials occur
# Trial locations and dates
trials <- read_csv("data/historical/trials_noI.csv", col_types = cols()) %>% 
  filter(Year >= 1982, Year <= 2014) %>% 
  distinct(County, .keep_all = TRUE) %>% 
  dplyr::select(State:Latitude)

# Get PRISM-county mappings
prism_files <- list.files("data/ISIMIP3a/dailys/gfdl-esm4/ssp126", 
                          "*\\.csv") %>%
  str_extract("[0-9]{6}") %>% 
  as.integer()
prism <- list.files("data/historical/prism_subset", "*\\.csv", full.names = TRUE) %>%
  `[`(1) %>% 
  read_csv(col_types = cols()) %>%
  filter(Cell %in% prism_files) %>% 
  filter(COUNTYNS %in% trials$County) %>% 
  distinct(COUNTYNS, Cell) %>%
  group_by(COUNTYNS) %>%
  summarise(Cells = list(Cell)) %>%
  ungroup()

models <- list.dirs("data/ISIMIP3a/dailys", full.names = FALSE, 
                    recursive = FALSE)
scenarios <- list.dirs("data/ISIMIP3a/dailys/gfdl-esm4", 
                       full.names = FALSE, recursive = FALSE) %>% 
  `[`(-1)

hist <- mclapply(models, function(m) {
  map_df(scenarios, function(sc) {
    map_df(1:nrow(prism), function(i) {
      cat(m, sc, i, "\n")
      map_df(prism$Cells[[i]], function(j) {
            paste0("data/ISIMIP3a/dailys/", m, "/", sc, "/PRISM_", 
                   j, "_daily.csv") %>%
              read_csv(col_types = cols()) %>% 
              mutate(DATE = paste(YEAR, MONTH, DAY, sep = "-") %>% ymd(),
                     Season = case_when(
                       MONTH >= 3 & MONTH <= 5 ~ "Spring", 
                       MONTH >= 9 & MONTH <= 11 ~ "Fall", 
                       TRUE ~ "Other"
                     )) %>% 
              filter(Season != "Other", TMIN <= 0) %>% 
              pivot_wider(names_from = "Season", values_from = "DATE") %>% 
              group_by(YEAR) %>% 
              summarise(Start = max(yday(Spring), na.rm = TRUE) + 1, 
                        End = min(yday(Fall), na.rm = TRUE) - 1, 
                        .groups = "drop")
          }) %>% 
        group_by(YEAR) %>% 
        summarise(Start = mean(Start), 
                  End = mean(End), 
                  .groups = "drop") %>% 
        mutate(County = prism$COUNTYNS[i], 
               Model = m, 
               Scenario = sc)
    })
  })
}, mc.cores = 5L, mc.silent = FALSE) %>% 
  bind_rows()

write_csv(hist, "data/gcm_frost_free.csv")
