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

# # For testing
# args <- list("model" = "gfdl-esm4", "scenario" = "historical")

# Historical season lengths
hist_par <- read_csv("data/ISIMIP3a/gdd_season_length.csv", col_types = cols())

# Link PRISM grid cells to the counties where trials occur
trials <- read_csv("data/historical/trials_noI.csv", col_types = cols())
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

# Simulated planting dates
sim_dates <- paste0("data/ISIMIP3a/distributions/", 
                    args$model, "/", args$scenario, "/growing_seasons.csv") %>% 
  read_csv(col_types = cols()) %>%
  nest(data = c(YEAR_P, MONTH_P, DAY_P)) %>% 
  inner_join(hist_par, by = "County")  %>%
  inner_join(prism_counties, by = c("County" = "COUNTYNS"))


har_dates <- lapply(1:nrow(sim_dates), function(i) {
  cat(args$model, args$scenario, i, "/", nrow(sim_dates), "\n")
  file_list <- paste0("data/ISIMIP3a/dailys/", args$model, "/", 
                      args$scenario, "/PRISM_", as.integer(sim_dates$Cells[[i]]), "_daily.csv")
  
  dates <- sim_dates$data[[i]] %>% 
    rowwise() %>% 
    mutate(PDATE = list(paste(c_across(YEAR_P:DAY_P), collapse = "-") %>% ymd()), 
           HDATE = list(paste(YEAR_P, "9-30", sep = "-") %>% ymd())) %>% 
    ungroup() %>% 
    unnest(c(PDATE, HDATE)) %>% 
    rowwise() %>% 
    mutate(DATES = list(seq(PDATE, HDATE, 1))) %>% 
    ungroup() %>% 
    unnest(DATES) %>% 
    pull(DATES)
  
  file_list %>% 
    map_df(function(f) {
        read_csv(f, col_types = cols()) %>% 
          mutate(Date = paste(YEAR, MONTH, DAY, sep = "-") %>% 
                   ymd()) %>% 
          filter(Date %in% dates) %>% 
          select(YEAR:DAY, TMAX, TMIN)
      }) %>% 
    group_by(YEAR, MONTH, DAY) %>% 
    summarise(across(TMAX:TMIN, ~ mean(.x, na.rm = TRUE)), 
              .groups = "drop") %>% 
    mutate(across(TMAX:TMIN, ~ if_else(.x < 10, 10, .x))) %>% 
    mutate(TMAX = if_else(TMAX > 30, 30, TMAX), 
           GDD = 0.5*(TMAX + TMIN) - 10) %>% 
    group_by(YEAR) %>% 
    mutate(GDD = cumsum(GDD)) %>% 
    ungroup() %>% 
    mutate(GDD2 = abs(GDD - sim_dates$GDD[i])) %>% 
    group_by(YEAR) %>% 
    arrange(GDD2) %>% 
    slice(1L) %>% 
    ungroup() %>% 
    select(YEAR:DAY) %>% 
    rename(YEAR_H = YEAR, MONTH_H = MONTH, DAY_H = DAY)
})


sim_dates <- sim_dates %>% 
  select(County:data) %>% 
  unnest(c(data)) %>% 
  bind_cols(bind_rows(har_dates))

write_csv(sim_dates, paste0("data/ISIMIP3a/distributions/", 
                            args$model, "/", args$scenario, "/growing_seasons.csv"))
