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
prism_files <- list.files("data/historical/prism_distribution", "*\\.csv") %>%
  str_extract("[0-9]{6}") %>% 
  as.integer()
prism <- list.files("data/prism_subset", "*\\.csv", full.names = TRUE) %>%
  `[`(1) %>% 
  read_csv(col_types = cols()) %>%
  filter(Cell %in% prism_files) %>% 
  filter(COUNTYNS %in% trials$County) %>% 
  distinct(COUNTYNS, Cell) %>%
  group_by(COUNTYNS) %>%
  summarise(Cells = list(Cell)) %>%
  ungroup()

hist <- mclapply(1:nrow(prism), function(i) {
    cat(i, "\n")
    map_df(prism$Cells[[i]], function(j) {
          paste0("data/historical/prism_daily/PRISM_", j, "_daily.csv") %>%
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
      mutate(County = prism$COUNTYNS[i])
  }, mc.cores = nrow(prism), mc.silent = FALSE) %>% 
  bind_rows()

write_csv(hist, "data/historical_frost_free.csv")
