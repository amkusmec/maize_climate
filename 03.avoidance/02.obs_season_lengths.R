library(tidyverse)
library(lubridate)
library(parallel)


options(readr.show_progress = FALSE)
options(readr.show_col_types = FALSE)

### Link PRISM grid cells to the counties where trials occur
# Trial locations and dates
trials <- read_csv("data/historical/trials_noI.csv") %>% 
  mutate(LengthDays = as.integer(Harvested - Planted))

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



gdd_calc <- function(i) {
  trials_temp <- trials %>% 
    filter(County == prism_counties$COUNTYNS[i])
  dates <- trials_temp %>% 
    rowwise() %>% 
    mutate(Dates = list(seq(Planted, Harvested, 1))) %>% 
    ungroup() %>% 
    unnest(Dates) %>% 
    pull(Dates)
  
  prism_counties$Cells[[i]] %>% 
    map_df(function(cc) {
        paste0("data/historical/prism_dailys/PRISM_", cc, "_daily.csv") %>% 
          read_csv() %>% 
          mutate(Date = paste(YEAR, MONTH, DAY, sep = "-") %>% 
                   ymd()) %>% 
          filter(Date %in% dates) %>% 
          select(Date, TMAX, TMIN)
      }) %>% 
    group_by(Date) %>% 
    summarise(across(TMAX:TMIN, ~ mean(.x, na.rm = TRUE)), 
              .groups = "drop") %>% 
    mutate(across(TMAX:TMIN, ~ if_else(.x < 10, 10, .x))) %>% 
    mutate(TMAX = if_else(TMAX > 30, 30, TMAX), 
           GDD = 0.5*(TMAX + TMIN) - 10, 
           YEAR = year(Date)) %>% 
    group_by(YEAR) %>% 
    summarise(GDD = sum(GDD), 
              .groups = "drop") %>% 
    mutate(County = prism_counties$COUNTYNS[i]) %>% 
    select(County, everything())
}

gdd <- mclapply(1:nrow(prism_counties), gdd_calc, mc.preschedule = FALSE, 
                mc.cores = 20L)

### Errors are from counties in the PRISM dataset that have no retained trials
classes <- lapply(gdd, class) %>% 
  sapply(`[[`, 1)
idx <- which(classes == "try-error")
gdd <- gdd[-idx] %>% 
  bind_rows()

county_gdd <- trials %>% 
  inner_join(gdd, by = c("Year" = "YEAR", "County")) %>% 
  group_by(County) %>% 
  summarise(GDD = median(GDD), 
            .groups = "drop")

write_csv(county_gdd, "data/ISIMIP3a/gdd_season_length.csv")
