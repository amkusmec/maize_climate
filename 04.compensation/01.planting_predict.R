library(tidyverse)
library(lubridate)
library(parallel)

options(readr.show_progress = FALSE)


# Empirically-derived planting date rules
hist_par <- read_csv("data/ISIMIP3a/selected_planting_windows.csv", 
                     col_types = cols())

### Link PRISM grid cells to the counties where trials occur
# Trial locations and dates
trials <- read_csv("data/historical/trials_noI.csv", 
                   col_types = cols())

# Two counties are present in all 31 years, so this will have 170 rows
missing <- expand_grid(County = unique(trials$County), 
                       Year = 1983:2013) %>% 
  anti_join(distinct(trials, Year, State, County), by = c("Year", "County")) %>% 
  inner_join(select(trials, State:Latitude) %>% 
               distinct(State, County, .keep_all = TRUE), by = "County") %>% 
  nest(data = !County)

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


dates <- lapply(1:nrow(missing), function(i) {
  cat("Trial", i, "/", nrow(missing), "\n")
  cidx <- which(hist_par$County == missing$County[i])
  
  min_yr <- min(missing$data[[i]]$Year) - hist_par$WINDOW[cidx]
  file_list <- paste0("data/historical/prism_dailys/PRISM_", as.integer(hist_par$Cells[[cidx]]), "_daily.csv")
  
  if(!all(file.exists(file_list))) file_list <- file_list[file.exists(file_list)]
  
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
  
  tibble(County = hist_par$County[cidx], 
         YEAR = missing$data[[i]]$Year) %>% 
    mutate(DF = map(YEAR, function(y) {
        tt <- temp %>% 
          filter(YEAR >= y - hist_par$WINDOW[cidx], YEAR < y) %>% 
          group_by(MONTH, DAY) %>% 
          summarise(FROST = mean(FROST, na.rm = TRUE), 
                    .groups = "drop") %>% 
          mutate(P = sign(0.05 - FROST))
        
        if (nrow(filter(tt, P != -1)) == 0) {
          tibble_row(MONTH = 6, DAY = 1)
        } else {
          tt %>% 
            filter(P != -1) %>% 
            slice(1L) %>% 
            select(MONTH:DAY)
        }
      })) %>% 
    unnest(DF)
})

dates <- bind_rows(dates) %>% 
  rename(YEAR_P = YEAR, MONTH_P = MONTH, DAY_P = DAY)
trials2 <- trials %>% 
  filter(Year >= 1983, Year <= 2013) %>% 
  mutate(YEAR_P = year(Planted), 
         MONTH_P = month(Planted), 
         DAY_P = day(Planted)) %>% 
  select(County, contains("_"))

write_csv(bind_rows(dates, trials2), paste0("data/ISIMIP3a/", 
                                            "adapted/distributions", 
                                            "/base_season_parameters.csv"))
