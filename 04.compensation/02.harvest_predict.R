library(tidyverse)
library(lubridate)


trials <- read_csv("data/historical/trials_noI.csv", col_types = cols()) %>% 
  mutate(YDAY = yday(Harvested))

m_h <- lm(YDAY ~ 1 + County + poly(Year, degree = 1):State, data = trials)

seasons <- read_csv(paste0("data/ISIMIP3a/adapted/distributions", 
                           "/base_season_parameters.csv"), col_types = cols()) %>% 
  inner_join(distinct(trials, State, County), by = "County") %>% 
  rename(Year = YEAR_P)
seasons$HDAY <- predict(m_h, newdata = seasons) %>% unname() %>% floor()

seasons <- seasons %>% 
  rename(YEAR_P = Year) %>% 
  mutate(HDATE = ymd(paste(YEAR_P, "01-01", sep = "-")) + HDAY, 
         YEAR_H = year(HDATE), 
         MONTH_H = month(HDATE), 
         DAY_H = day(HDATE)) %>% 
  select(State, County, contains("_"))

write_csv(seasons, paste0("data/ISIMIP3a/adapted/distributions", 
                          "/base_season_parameters.csv"))
