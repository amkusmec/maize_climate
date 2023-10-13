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
# args <- list("model" = "ipsl-cm6a-lr", "scenario" = "ssp370")
# args <- list("model" = "ukesm1-0-ll", "scenario" = "ssp126")

### Predicted planting/harvesting dates
dir1 <- paste0("data/ISIMIP3a/adapted/distributions/", args$model)
dir2 <- paste0(dir1, "/", args$scenario)
if (!dir.exists(dir2)) dir.create(dir2)

if (args$scenario == "historical") {
  hist_par <- paste0("data/ISIMIP3a/adapted/distributions/", 
                     "base_season_parameters.csv") %>% 
    read_csv(col_types = cols()) %>% 
    mutate(Planted = paste(YEAR_P, MONTH_P, DAY_P, sep = "-") %>% ymd(), 
           Harvested = paste(YEAR_H, MONTH_H, DAY_H, sep = "-") %>% ymd()) %>% 
    select(County, Planted:Harvested)
} else {
  hist_par <- paste0(dir1, "/", args$scenario, "/adapted_seasons.csv") %>% 
    read_csv(col_types = cols()) %>% 
    rename(Planted = DATE_P, Harvested = DATE_H) %>% 
    select(County, Planted:Harvested)
}


### Link PRISM grid cells to the counties where trials occur
# Trial locations and dates
trials <- read_csv("data/historical/trials_noI.csv", 
                   col_types = cols())

# Get PRISM-county mappings
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

# Combine county-cell mappings and parameters
hist_par <- hist_par %>% 
  nest(data = !County) %>% 
  inner_join(prism_counties, by = c("County" = "COUNTYNS"))

rm(trials, prism_files, prism_counties); gc()

# Function for computing daily temperature exposure distributions
temp_dist <- compiler::cmpfun(function(df) {
  tmin <- df$TMIN[1]; tmax <- df$TMAX[1]
  
  if (floor(tmin) >= floor(tmax)) {
    tibble(Degree = floor(tmin), 
           Time = 24)
  } else {
    M <- (tmax + tmin)/2
    W <- (tmax - tmin)/2
    
    temp_grid <- seq(ceiling(tmin), floor(tmax))
    time_grid <- asin((temp_grid - M)/W)
    hour_grid <- c(0, 12*time_grid/pi + 6, 12)
    
    tibble(Degree = c(min(temp_grid) - 1, temp_grid), 
           Time = 2*diff(hour_grid))
  }
})

# Thresholds for censoring the distributions
temp_left <- 0
temp_right <- 40

prcp_left <- 250
prcp_right <- 800

# Main loop
for (i in 1:nrow(hist_par)) {
  cat("County", i, "/", nrow(hist_par), "\n")
  
  file_list <- paste0("data/ISIMIP3a/dailys/", args$model, 
                      "/", args$scenario, "/PRISM_", hist_par$Cells[[i]], "_daily.csv")
  if(!all(file.exists(file_list))) file_list <- file_list[file.exists(file_list)]
  
  dates <- hist_par$data[[i]] %>% 
    rowwise() %>% 
    mutate(dates = list(seq(Planted, Harvested, 1))) %>% 
    unnest(dates) %>% 
    pull(dates)
  
  dists <- mclapply(file_list, function(f) {
    dailys <- read_csv(f, col_types = cols()) %>% 
      mutate(DATE = ymd(paste(YEAR, MONTH, DAY, sep = "-"))) %>% 
      filter(DATE %in% dates)
    
    prcp <- dailys %>% 
      group_by(YEAR) %>% 
      summarise(PRCP = sum(PRCP, na.rm = TRUE), 
                .groups = "drop")
    
    temp <- dailys %>% 
      select(YEAR, DATE, TMIN, TMAX) %>% 
      purrrlyr::by_row(temp_dist, .collate = "list", .to = "Distribution") %>% 
      unnest(Distribution) %>% 
      group_by(YEAR, Degree) %>% 
      summarise(Time = sum(Time, na.rm = TRUE), 
                .groups = "drop")
    
    list(PRCP = prcp, TEMP = temp)
  }, mc.cores = 50L, mc.preschedule = FALSE)
  
  prcp <- lapply(dists, `[[`, 1) %>% 
    bind_rows() %>% 
    group_by(YEAR) %>% 
    summarise(PRCP = mean(PRCP, na.rm = TRUE), 
              .groups = "drop") %>% 
    mutate(PRCP = if_else(PRCP < prcp_left, prcp_left, PRCP), 
           PRCP = if_else(PRCP > prcp_right, prcp_right, PRCP), 
           County = hist_par$County[i]) %>% 
    select(County, everything())
  
  temp <- lapply(dists, `[[`, 2) %>% 
    bind_rows() %>% 
    group_by(YEAR, Degree) %>% 
    summarise(Time = sum(Time, na.rm = TRUE)/length(file_list), 
              .groups = "drop") %>% 
    group_by(YEAR) %>% 
    group_modify(~ {
        temp_low <- filter(.x, Degree < temp_left) %>%
          pull(Time) %>%
          sum(na.rm = TRUE)
        temp_high <- filter(.x, Degree > temp_right) %>%
          pull(Time) %>%
          sum(na.rm = TRUE)
        temp <- filter(.x, Degree >= temp_left, 
                       Degree <= temp_right) %>%
          bind_rows(tibble(Degree = c(temp_left - 1, temp_right + 1), 
                           Time = c(temp_low, temp_high)))
        
        missing <- setdiff((temp_left - 1):(temp_right + 1), temp$Degree)
        if (length(missing) > 0) {
          temp <- bind_rows(temp, 
                            tibble(Degree = missing, 
                                   Time = 0))
        }
        
        arrange(temp, Degree)
      }) %>% 
    ungroup() %>% 
    mutate(County = hist_par$County[i]) %>% 
    select(County, everything())
  
  write_csv(prcp, paste0(dir2, "/county_", hist_par$County[i], "_prcp.csv"))
  write_csv(temp, paste0(dir2, "/county_", hist_par$County[i], "_temp.csv"))
}
