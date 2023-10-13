library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)
source("src/cal_scales.R")


models <- list.dirs("data/ISIMIP3a/adapted/distributions", 
                    recursive = FALSE, full.names = FALSE)
scenarios <- list.dirs(paste0("data/ISIMIP3a/adapted/distributions/", 
                              models[1]), recursive = FALSE, full.names = FALSE) %>% 
  `[`(-1)


plant_only <- map_df(models, function(m) {
  map_df(scenarios, function(s) {
    paste0("data/ISIMIP3a/distributions/", m, "/", s, 
           "/growing_seasons.csv") %>% 
      read_csv(col_types = cols()) %>% 
      mutate(DATE_P = paste(YEAR_P, MONTH_P, DAY_P, sep = "-") %>% ymd(), 
             DATE_H = paste(YEAR_H, MONTH_H, DAY_H, sep = "-") %>% ymd(), 
             PDAY = yday(DATE_P), 
             HDAY = yday(DATE_H), 
             GSL = as.integer(DATE_H - DATE_P), 
             Model = m, 
             Scenario = s, 
             Adaptation = "Planting only") %>% 
      rename(Year = YEAR_P) %>% 
      select(Model:Adaptation, Year, County, PDAY:GSL)
  })
})

plant_gdd <- map_df(models, function(m) {
  map_df(scenarios, function(s) {
    paste0("data/ISIMIP3a/adapted/distributions/", m, "/", s, 
           "/adapted_seasons.csv") %>% 
      read_csv(col_types = cols()) %>% 
      mutate(PDAY = yday(DATE_P), 
             HDAY = yday(DATE_H), 
             Model = m, 
             Scenario = s, 
             Adaptation = "Planting + GDD") %>% 
      select(Model:Adaptation, Year, County, PDAY:HDAY, GSL)
  })
})

opt_cycle <- map_df(models, function(m) {
  map_df(scenarios, function(s) {
    paste0("data/ISIMIP3a/cycle_adapted/distributions/", m, "/", s, 
           "/season_calendar.csv") %>% 
      read_csv(col_types = cols()) %>% 
      mutate(Model = m, 
             Scenario = s, 
             Adaptation = "Optimal cycle", 
             GSL = as.integer(HDAY - PDAY)) %>% 
      select(Model:Adaptation, Year, County, PDAY:HDAY, GSL)
  })
})

ref <- map_df(models, function(m) {
  map_df(scenarios, function(s) {
    paste0("data/ISIMIP3a/reference_cal/distributions/", m, "/", 
           s, "/estimated_season_lengths.csv") %>% 
      read_csv(col_types = cols()) %>% 
      rename(Year = YEAR) %>% 
      mutate(GSL = HDAY - PDAY, 
             Adaptation = "Reference") %>% 
      select(Model, Scenario, Adaptation, Year, County, PDAY:GSL)
  })
})


calendars <- bind_rows(plant_only, plant_gdd, opt_cycle, ref) %>% 
  filter(Year < 2100)


periods <- calendars %>% 
  filter(Year >= 2040, Year <= 2099) %>% 
  mutate(Period = if_else(Year <= 2069, "2040-2069", "2070-2099")) %>% 
  group_by(Adaptation, Scenario, Period, Year, County) %>% 
  summarise(across(PDAY:GSL, ~ mean(.x, na.rm = TRUE)), 
            .groups = "drop_last") %>% 
  summarise(across(PDAY:GSL, ~ mean(.x, na.rm = TRUE)), 
            .groups = "drop_last") %>% 
  summarise(across(PDAY:GSL, ~ mean(.x, na.rm = TRUE)), 
            .groups = "drop") %>% 
  pivot_wider(names_from = "Period", values_from = PDAY:GSL, names_sep = "_") %>% 
  mutate(Adaptation = recode_factor(Adaptation, "Planting + GDD" = "Planting & maturity") %>% 
           factor(levels = names(cal_sc), ordered = TRUE))

cal_shifts <- calendars %>% 
  filter(Year >= 2040) %>% 
  filter(Adaptation != "Reference") %>% 
  inner_join(filter(calendars, Adaptation == "Reference"), 
             by = c("Model", "Scenario", "Year", "County")) %>% 
  mutate(PDAY_shift = PDAY.x - PDAY.y, 
         HDAY_shift = HDAY.x - HDAY.y, 
         GSL_shift = GSL.x - GSL.y, 
         Period = if_else(Year <= 2069, "2040-2069", "2070-2099")) %>% 
  select(Model:County, contains("shift"), Period) %>% 
  group_by(Adaptation.x, Scenario, Period, Year, County) %>% 
  summarise(across(contains("shift"), ~ mean(.x, na.rm = TRUE)), 
            .groups = "drop_last") %>% 
  summarise(across(contains("shift"), ~ mean(.x, na.rm = TRUE)), 
            .groups = "drop_last") %>% 
  summarise(across(contains("shift"), ~ mean(.x, na.rm = TRUE)), 
            .groups = "drop") %>% 
  pivot_wider(names_from = "Period", values_from = PDAY_shift:GSL_shift, names_sep = "_") %>% 
  mutate(Adaptation.x = recode_factor(Adaptation.x, "Planting + GDD" = "Planting & maturity") %>% 
           factor(levels = names(cal_sc), ordered = TRUE))

pA <- ggplot(cal_shifts) + theme_classic() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  geom_segment(aes(x = `PDAY_shift_2040-2069`, y = `HDAY_shift_2040-2069`, 
                   xend = `PDAY_shift_2070-2099`, yend = `HDAY_shift_2070-2099`, 
                   colour = Adaptation.x), linetype = 1, size = 0.5) + 
  geom_point(aes(x = `PDAY_shift_2070-2099`, y = `HDAY_shift_2070-2099`, 
                 colour = Adaptation.x, shape = Scenario), size = 4, fill = "white", 
             stroke = 1) + 
  scale_colour_manual(values = cal_sc) + 
  scale_shape_manual(labels = c("ssp126" = "SSP1-2.6",
                                   "ssp370" = "SSP3-7.0",
                                   "ssp585" = "SSP5-8.5"), 
                        values = c("ssp126" = 21, "ssp370" = 24, 
                                   "ssp585" = 22)) +
  labs(x = "Planting date shift (d)", y = "Maturity date shift (d)", 
       colour = "Adaptation", tag = "A") + 
  theme(axis.title = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"))


pB <- ggplot(periods) + theme_classic() + 
  geom_segment(aes(x = factor(1), xend = factor(2), 
                   y = `GSL_2040-2069`, yend = `GSL_2070-2099`, 
                   colour = Adaptation), 
               linetype = 1, size = 0.5) + 
  geom_point(aes(x = factor(2), y = `GSL_2070-2099`, shape = Scenario, 
                 colour = Adaptation), size = 4, fill = "white", stroke = 1) + 
  scale_colour_manual(values = cal_sc) + 
  scale_shape_manual(labels = c("ssp126" = "SSP1-2.6",
                                   "ssp370" = "SSP3-7.0",
                                   "ssp585" = "SSP5-8.5"), 
                        values = c("ssp126" = 21, "ssp370" = 24, 
                                   "ssp585" = 22)) +
  scale_x_discrete(labels = c("2040-2069", "2070-2099")) + 
  labs(x = "", y = "Growing season length (d)", tag = "B") + 
  theme(axis.title = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"))

gp <- arrangeGrob(ggplotGrob(pA), ggplotGrob(pB), 
                  layout_matrix = matrix(1:2, nrow = 1))
png("figures/Fig_calendar_summary.png", width = 9, height = 3.5, 
    units = "in", res = 200)
plot(gp)
dev.off()



cal_shifts1 <- calendars %>% 
  filter(Year >= 2040) %>% 
  filter(Adaptation != "Reference") %>% 
  inner_join(filter(calendars, Adaptation == "Reference"), 
             by = c("Model", "Scenario", "Year", "County")) %>% 
  mutate(PDAY_shift = PDAY.x - PDAY.y, 
         HDAY_shift = HDAY.x - HDAY.y, 
         GSL_shift = GSL.x - GSL.y, 
         Period = if_else(Year <= 2069, "2040-2069", "2070-2099")) %>% 
  select(Model:County, contains("shift"), Period) %>% 
  group_by(Adaptation.x, Scenario, Model, Period, Year) %>% 
  summarise(across(contains("shift"), ~ mean(.x, na.rm = TRUE)), 
            .groups = "drop_last") %>% 
  summarise(across(contains("shift"), ~ mean(.x, na.rm = TRUE)), 
            .groups = "drop") %>%
  pivot_wider(names_from = "Period", values_from = PDAY_shift:GSL_shift, names_sep = "_") %>% 
  mutate(Adaptation.x = recode_factor(Adaptation.x, "Planting + GDD" = "Planting & maturity") %>% 
           factor(levels = names(cal_sc), ordered = TRUE))


p1 <- ggplot(cal_shifts1) + theme_classic() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) + 
  geom_segment(aes(x = `PDAY_shift_2040-2069`, y = `HDAY_shift_2040-2069`, 
                   xend = `PDAY_shift_2070-2099`, yend = `HDAY_shift_2070-2099`, 
                   colour = Adaptation.x), linetype = 1, size = 0.5) + 
  geom_point(aes(x = `PDAY_shift_2070-2099`, y = `HDAY_shift_2070-2099`, 
                 colour = Adaptation.x, shape = Scenario), size = 4, fill = "white", 
             stroke = 1) + 
  scale_colour_manual(values = cal_sc) + 
  scale_shape_manual(labels = c("ssp126" = "SSP1-2.6",
                                "ssp370" = "SSP3-7.0",
                                "ssp585" = "SSP5-8.5"), 
                     values = c("ssp126" = 21, "ssp370" = 24, 
                                "ssp585" = 22)) +
  facet_wrap(~ Model, nrow = 1, 
             labeller = labeller(Model = as_labeller(model_lbls, default = label_parsed))) +
  labs(x = "Planting date shift (d)", y = "Maturity date shift (d)", 
       colour = "Adaptation", tag = "A") + 
  theme(axis.title = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"))


periods1 <- calendars %>% 
  filter(Year >= 2040, Year <= 2099) %>% 
  mutate(Period = if_else(Year <= 2069, "2040-2069", "2070-2099")) %>% 
  group_by(Adaptation, Scenario, Model, Period, Year) %>% 
  summarise(across(PDAY:GSL, ~ mean(.x, na.rm = TRUE)), 
            .groups = "drop_last") %>% 
  summarise(across(PDAY:GSL, ~ mean(.x, na.rm = TRUE)), 
            .groups = "drop") %>% 
  pivot_wider(names_from = "Period", values_from = PDAY:GSL, names_sep = "_") %>%
  mutate(Adaptation = recode_factor(Adaptation, "Planting + GDD" = "Planting & maturity") %>% 
           factor(levels = names(cal_sc), ordered = TRUE))

p2 <- ggplot(periods1) + theme_classic() + 
  geom_segment(aes(x = factor(1), xend = factor(2), y = `GSL_2040-2069`, 
                   yend = `GSL_2070-2099`, colour = Adaptation), 
               size = 0.5, linetype = 1) + 
  geom_point(aes(x = factor(2), y = `GSL_2070-2099`, shape = Scenario, 
                 colour = Adaptation), size = 4, fill = "white", stroke = 1) + 
  scale_colour_manual(values = cal_sc) + 
  scale_shape_manual(labels = c("ssp126" = "SSP1-2.6",
                                "ssp370" = "SSP3-7.0",
                                "ssp585" = "SSP5-8.5"), 
                     values = c("ssp126" = 21, "ssp370" = 24, 
                                "ssp585" = 22)) +
  scale_x_discrete(labels = c("2040-2069", "2070-2099")) + 
  facet_wrap(~ Model, nrow = 1, 
             labeller = labeller(Model = as_labeller(model_lbls, default = label_parsed))) + 
  labs(x = "", y = "Growing season length (d)", tag = "B") + 
  theme(axis.title = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"))

gp2 <- arrangeGrob(ggplotGrob(p1), ggplotGrob(p2), 
                   layout_matrix = matrix(1:2, ncol = 1))
png("figures/FigS_calendar_models.png", width = 10, height = 5.5, 
    units = "in", res = 200)
plot(gp2)
dev.off()
