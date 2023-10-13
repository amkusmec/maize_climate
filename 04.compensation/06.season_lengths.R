library(tidyverse)
library(lubridate)
library(parallel)
library(argparse)

options(readr.show_progress = FALSE)


# Parse commandline arguments
parser <- ArgumentParser()
parser$add_argument("-m", "--model", type = "character")
parser$add_argument("-s", "--scenario", type = "character")
args <- parser$parse_args()

# For testing
# args <- list("model" = "gfdl-esm4", "scenario" = "ssp126")

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

pdates <- read_csv(paste0("data/ISIMIP3a/distributions/", 
                          args$model, "/", args$scenario, "/growing_seasons.csv")) %>% 
  mutate(DATE_P = paste(YEAR_P, MONTH_P, DAY_P, sep = "-") %>% ymd()) %>% 
  select(County, YEAR_P, DATE_P)

seasons <- read_csv(paste0("data/ISIMIP3a/adapted/", 
                           "distributions/", args$model, "/adapted_gdds.csv")) %>% 
  inner_join(pdates, by = c("County", "Year" = "YEAR_P")) %>% 
  select(-State) %>% 
  nest(data = !County) %>% 
  inner_join(prism_counties, by = c("County" = "COUNTYNS"))

gdd <- lapply(1:nrow(seasons), function(i) {
  cat("County", i, "/", nrow(seasons), "\n")
  
  file_list <- paste0("data/ISIMIP3a/dailys/", args$model, 
                      "/", args$scenario, "/PRISM_", seasons$Cells[[i]], "_daily.csv")
  if(!all(file.exists(file_list))) file_list <- file_list[file.exists(file_list)]
  
  gdd_cells <- file_list %>% 
    map_df(function(f) {
        read_csv(f, col_types = cols()) %>% 
          mutate(CELL = str_extract(f, "[0-9]{6}"), 
                 DATE = paste(YEAR, MONTH, DAY, sep = "-") %>% 
                   ymd()) %>% 
          mutate(across(TMAX:TMIN, ~ if_else(.x < 10, 10, .x))) %>% 
          mutate(across(TMAX:TMIN, ~ if_else(.x > 30, 30, .x))) %>% 
          mutate(GDD = 0.5*(TMAX + TMIN) - 10) %>% 
          select(CELL:DATE, GDD)
      }) %>% 
    group_by(DATE) %>% 
    summarise(GDD = mean(GDD, na.rm = TRUE), 
              .groups = "drop")
  
  seasons$data[[i]] %>% 
    mutate(DATE_H = map2(GDDnew, DATE_P, function(gg, dd) {
        gdd_cells %>% 
          filter(DATE >= dd) %>% 
          mutate(GDD = cumsum(GDD)) %>% 
          filter(GDD >= gg) %>%
          slice(1L) %>% 
          pull(DATE)
      })) %>% 
    unnest(DATE_H) %>% 
    mutate(DATE_H = if_else((month(DATE_H) >= 11 & day(DATE_H) > 1) | (year(DATE_H) != year(DATE_P)), 
                            paste(year(DATE_P), "11", "01", sep = "-") %>% ymd(), 
                            DATE_H))
})

seasons <- seasons %>% 
  select(County) %>% 
  mutate(data = gdd) %>% 
  unnest(data) %>% 
  mutate(GSL = as.integer(DATE_H - DATE_P))

dir2 <- paste0(dir1, args$scenario, "/")
if(!dir.exists(dir2)) dir.create(dir2)

write_csv(seasons, paste0(dir2, "adapted_seasons.csv"))
