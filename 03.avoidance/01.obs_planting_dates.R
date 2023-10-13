library(tidyverse)
library(lubridate)
library(parallel)

# From Parent et al.:
# first day from January to May with probability of frost <5% over the next 
# 10 d calculated over 36 y

options(readr.show_progress = FALSE)

### Link PRISM grid cells to the counties where trials occur
# Trial locations and dates
trials <- read_csv("data/historical/trials_noI.csv")

# Get PRISM-county mappings
prism_files <- list.files("data/historical/prism_dailys", "*\\.csv") %>%
  str_split("_") %>%
  map_chr(`[`, 2) %>%
  as.integer()
prism_counties <- list.files("data/historical/prism_subset", "*\\.csv", full.names = TRUE) %>%
  `[`(1) %>% 
  read_csv() %>%
  filter(Cell %in% prism_files) %>%
  distinct(COUNTYNS, Cell) %>%
  group_by(COUNTYNS) %>%
  summarise(Cells = list(Cell), 
            .groups = "drop")

frost_prob <- function(x, horizon = 10) {
  res <- rep(as.numeric(NA), length(x))
  for (i in 1:(length(x) - horizon)) {
    res[i] <- mean(x[(i + 1):(i + horizon)])
  }
  res
}

frost_thresh <- -5:5
window_sizes <- seq(5, 50, 5)

date_predict <- function(rr) {
  trials_temp <- trials %>% 
    filter(County == prism_counties$COUNTYNS[rr])
  
  temp <- prism_counties$Cells[[rr]] %>% 
    map_df(function(i) {
        paste0("data/historical/prism_dailys/PRISM_", i, "_daily.csv") %>% 
          read_csv(show_col_types = FALSE) %>% 
          filter(MONTH < 6 | (MONTH == 6 & DAY <= 10)) %>% 
        filter(!(MONTH == 2 & DAY > 28)) %>% 
        filter(!(MONTH == 4 & DAY > 30))
      }) %>% 
    group_by(YEAR, MONTH, DAY) %>% 
    group_modify(~ {
        tibble(THRESHOLD = frost_thresh) %>% 
          mutate(FROST = map_int(THRESHOLD, function(x) if_else(mean(.x$TMIN, na.rm = TRUE) < x, 1L, 0L)))
      }) %>% 
    ungroup() %>% 
    group_by(THRESHOLD, YEAR) %>% 
    mutate(FROST = frost_prob(FROST)) %>% 
    ungroup() %>% 
    filter(!is.na(FROST))
  
  min_year <- min(temp$YEAR)
  
  pred <- expand_grid(YEAR = trials_temp$Year, 
                      THRESHOLD = frost_thresh, 
                      WINDOW = window_sizes) %>% 
    filter(YEAR - WINDOW >= min_year) %>% 
    mutate(Pred_Date = as_date(NA))
  if (nrow(pred) == 0) return(NULL)
  
  for (i in 1:nrow(pred)) {
    pred$Pred_Date[i] <- temp %>% 
      filter(YEAR < pred$YEAR[i], YEAR >= pred$YEAR[i] - pred$WINDOW[i]) %>% 
      filter(THRESHOLD == pred$THRESHOLD[i]) %>% 
      group_by(MONTH, DAY) %>% 
      summarise(FROST = mean(FROST, na.rm = TRUE), 
                .groups = "drop") %>% 
      mutate(P = sign(0.05 - FROST)) %>% 
      filter(P != -1) %>% 
      slice(1L) %>% 
      select(MONTH:DAY) %>% 
      unlist() %>% 
      paste(collapse = "-") %>% 
      paste(pred$YEAR[i], sep = "-") %>% 
      mdy()
  }
  
  inner_join(pred, trials_temp, by = c("YEAR" = "Year")) %>% 
    select(County, YEAR:Pred_Date , Planted) %>% 
    mutate(Error = as.numeric(Planted - Pred_Date)) %>% 
    group_by(County, THRESHOLD, WINDOW) %>% 
    summarise(MAE = median(abs(Error)), 
              .groups = "drop")
}

predicted_dates <- mclapply(1:nrow(prism_counties), date_predict, 
                            mc.cores = 20L, mc.preschedule = FALSE)
predicted_dates <- predicted_dates[!sapply(predicted_dates, is.null)]
predicted_dates <- bind_rows(predicted_dates)
write_csv(predicted_dates, "data/ISIMIP3a/predicted_planting_dates.csv")

sel <- predicted_dates %>% 
  group_by(County) %>% 
  arrange(MAE) %>% 
  slice(1L) %>% 
  ungroup()

source("src/haversine.R")
counties <- trials %>% 
  distinct(County, .keep_all = TRUE) %>% 
  select(County, Longitude, Latitude)
sel2 <- anti_join(counties, sel, by = "County") %>% 
  mutate(Nearest = map2(Longitude, Latitude, function(x, y) {
      counties %>% 
        mutate(Distance = haversine(x, Longitude, y, Latitude)) %>% 
        arrange(Distance) %>% 
        slice(2:5L) %>% 
        rename(County2 = County)
    })) %>% 
  unnest(Nearest, names_repair = "unique") %>% 
  select(County, County2, Distance) %>% 
  inner_join(sel, by = c("County2" = "County")) %>% 
  group_by(County) %>% 
  mutate(wt = 1/Distance^2, 
         wt = wt/sum(wt)) %>% 
  summarise(THRESHOLD = sum(wt*THRESHOLD), 
            WINDOW = sum(wt*WINDOW), 
            .groups = "drop") %>% 
  mutate(across(THRESHOLD:WINDOW, ~ round(.x))) %>% 
  mutate(MAE = as.numeric(NA))

sel <- bind_rows(sel, sel2)
write_csv(sel, "data/ISIMIP3a/selected_planting_windows.csv")
