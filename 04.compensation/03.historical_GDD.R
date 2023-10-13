library(tidyverse)
library(lubridate)
library(parallel)
library(argparse)

options(readr.show_progress = FALSE)


# Parse commandline arguments
parser <- ArgumentParser()
parser$add_argument("-m", "--model", type = "character")
args <- parser$parse_args()

# For testing
# args <- list("model" = "gfdl-esm4")

# Prepare directory structure
dir1 <- paste0("data/ISIMIP3a/adapted/distributions/", 
               args$model, "/")
if (!dir.exists(dir1)) dir.create(dir1)

# Link PRISM grid cells to the counties where trials occur
trials <- read_csv("data/historical/trials_noI.csv", 
                   col_types = cols())
prism_files <- list.files("data/historical/prism_dailys", "*\\.csv") %>%
  str_split("_") %>%
  map_chr(`[`, 2) %>%
  as.integer()
prism_counties <- list.files("data/historical/prism_subset", "*\\.csv", full.names = TRUE) %>%
  `[`(1) %>% 
  read_csv(col_types = cols()) %>%
  filter(Cell %in% prism_files) %>%
  distinct(COUNTYNS, Cell) %>%
  group_by(COUNTYNS) %>%
  summarise(Cells = list(Cell), 
            .groups = "drop")

seasons <- read_csv(paste0("data/ISIMIP3a/adapted/", 
                           "distributions/base_season_parameters.csv")) %>% 
  mutate(Year = YEAR_P, 
         Planted = paste(YEAR_P, MONTH_P, DAY_P, sep = "-") %>% ymd(), 
         Harvested = paste(YEAR_H, MONTH_H, DAY_H, sep = "-") %>% ymd()) %>% 
  select(Year, State:County, Planted, Harvested) %>% 
  arrange(Year, State, County) %>% 
  nest(data = !County) %>% 
  inner_join(prism_counties, by = c("County" = "COUNTYNS"))

gdd <- lapply(1:nrow(seasons), function(i) {
  cat("County", i, "/", nrow(seasons), "\n")
  
  file_list <- paste0("data/ISIMIP3a/dailys/", args$model, 
                      "/historical/PRISM_", seasons$Cells[[i]], "_daily.csv")
  if(!all(file.exists(file_list))) file_list <- file_list[file.exists(file_list)]
  
  dates <- seasons$data[[i]] %>% 
    rowwise() %>% 
    mutate(DATES = list(seq(Planted, Harvested, 1))) %>% 
    ungroup() %>% 
    unnest(DATES) %>% 
    pull(DATES)
  
  file_list %>% 
    map_df(function(f) {
      read_csv(f, col_types = cols()) %>% 
          mutate(Date = paste(YEAR, MONTH, DAY, sep = "-") %>% 
                   ymd()) %>% 
          filter(Date %in% dates) %>% 
          select(YEAR:DAY, TMAX, TMIN) %>% 
          mutate(across(TMAX:TMIN, ~ if_else(.x < 10, 10, .x))) %>% 
          mutate(across(TMAX:TMIN, ~ if_else(.x > 30, 30, .x))) %>% 
          mutate(GDD = 0.5*(TMAX + TMIN) - 10) %>% 
          group_by(YEAR) %>% 
          summarise(GDD = sum(GDD), 
                    .groups = "drop")
      }) %>% 
    group_by(YEAR) %>% 
    summarise(GDD = mean(GDD, na.rm = TRUE), 
              .groups = "drop") %>% 
    mutate(State = seasons$data[[i]]$State[1], 
           County = seasons$County[i]) %>% 
    select(State:County, everything())
})

gdd <- bind_rows(gdd)

write_csv(gdd, paste0(dir1, "/estimated_season_lengths.csv"))
