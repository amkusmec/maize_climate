library(tidyverse)

reg_dat <- read_rds("data/historical/reg_dat_CUB.rds") %>% 
  mutate(Year = attr(Year, "scaled:scale")*Year + attr(Year, "scaled:center"))

all_hybrids <- reg_dat %>% 
  count(Variety) %>% 
  mutate(Wt = n/sum(n))

cohort_hybrids <- reg_dat %>% 
  group_by(Variety) %>% 
  summarise(Year = 5*(min(Year) %/% 5), 
            n = n(), 
            .groups = "drop") %>% 
  group_by(Year) %>% 
  mutate(Wt = n/sum(n)) %>% 
  ungroup()

group_hybrids <- reg_dat %>% 
  group_by(Variety) %>% 
  summarise(Year = min(Year), 
            n = n(), 
            .groups = "drop") %>% 
  mutate(Group = case_when(
      Year < min(Year) + 10 ~ "Early", 
      Year >= 1975 & Year <= 1985 ~ "Mid", 
      Year > max(Year) - 10 ~ "Late", 
      TRUE ~ "NA"
    )) %>% 
  filter(Group != "NA") %>% 
  group_by(Group) %>% 
  mutate(Wt = n/sum(n)) %>% 
  ungroup()

list("all" = all_hybrids, "cohort" = cohort_hybrids, "group" = group_hybrids) %>% 
  write_rds("data/ISIMIP3a/hybrid_counts.rds")
