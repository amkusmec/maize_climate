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
# args <- list("model" = "ipsl-cm6a-lr", "scenario" = "historical")

# Empirically-derived planting date rules
hist_par <- read_csv("data/ISIMIP3a/selected_planting_windows.csv", 
                     col_types = cols())

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
hist_par <- inner_join(hist_par, prism_counties, by = c("County" = "COUNTYNS"))

### Function to calculate frost probability
frost_prob <- function(x, horizon = 10) {
  res <- rep(as.numeric(NA), length(x))
  for (i in 1:(length(x) - horizon)) {
    res[i] <- mean(x[(i + 1):(i + horizon)])
  }
  res
}

# Prepare directory structure
dir1 <- paste0("data/ISIMIP3a/distributions/", args$model, "/")
if (!dir.exists(dir1)) dir.create(dir1)
dir1 <- paste0(dir1, args$scenario, "/")
if (!dir.exists(dir1)) dir.create(dir1)

### Begin with 1934 (the beginning of our dataset)
yr_start <- if_else(args$scenario == "historical", 1934L, 2015L)


dates <- lapply(1:nrow(hist_par), function(i) {
  cat(args$model, args$scenario, i, "/", nrow(hist_par), "\n")
  min_yr <- yr_start - hist_par$WINDOW[i]
  file_list <- paste0("data/ISIMIP3a/dailys/", args$model, "/", 
                      args$scenario, "/PRISM_", as.integer(hist_par$Cells[[i]]), "_daily.csv")
  if (args$scenario != "historical") {
    file_list <- append(file_list, 
                        str_replace(file_list, args$scenario, "historical"))
  }
  
  if(!all(file.exists(file_list))) return(file_list[!file.exists(file_list)])
  
  temp <- file_list %>% 
    map_df(function(f) {
        read_csv(f, show_col_types = FALSE) %>% 
          select(YEAR:DAY, TMIN) %>% 
          filter(MONTH < 6 | (MONTH == 6 & DAY <= 10)) %>% 
          filter(!(MONTH == 2 & DAY > 28)) %>% 
          filter(!(MONTH == 4 & DAY > 30)) %>% 
          filter(YEAR >= min_yr)
      }) %>% 
    group_by(YEAR, MONTH, DAY) %>% 
    summarise(FROST = if_else(mean(TMIN, na.rm = TRUE) < hist_par$THRESHOLD[i], 
                              1L, 0L), 
              .groups = "drop") %>% 
    arrange(YEAR, MONTH, DAY) %>% 
    group_by(YEAR) %>% 
    mutate(FROST = frost_prob(FROST)) %>% 
    ungroup() %>% 
    filter(!is.na(FROST))
  
  tibble(County = hist_par$County[i], 
                  YEAR = seq(yr_start, max(temp$YEAR))) %>% 
    mutate(DF = map(YEAR, function(y) {
        temp %>% 
          filter(YEAR >= y - hist_par$WINDOW[i], YEAR < y) %>% 
          group_by(MONTH, DAY) %>% 
          summarise(FROST = mean(FROST, na.rm = TRUE), 
                    .groups = "drop") %>% 
          mutate(P = sign(0.05 - FROST)) %>% 
          filter(P != -1) %>% 
          slice(1L) %>% 
          select(MONTH:DAY)
      })) %>% 
    unnest(DF)
})

dates <- bind_rows(dates) %>% 
  rename(YEAR_P = YEAR, MONTH_P = MONTH, DAY_P = DAY)

write_csv(dates, paste0("data/ISIMIP3a/distributions/", 
                        args$model, "/", args$scenario, "/growing_seasons.csv"))
