library(tidyverse)
library(lubridate)


models <- list.dirs("data/ISIMIP3a/adapted/distributions", 
                    recursive = FALSE, full.names = FALSE)
scenarios <- list.dirs(paste0("data/ISIMIP3a/adapted/distributions/", 
                              models[1]), recursive = FALSE, full.names = FALSE) %>% 
  `[`(-1)

hist <- read_csv(paste0("data/ISIMIP3a/adapted/", 
                        "distributions/base_season_parameters.csv"), 
                 col_types = cols()) %>% 
  mutate(DATE_P = paste(YEAR_P, MONTH_P, DAY_P, sep = "-") %>% ymd(), 
         DATE_H = paste(YEAR_H, MONTH_H, DAY_H, sep = "-") %>% ymd(), 
         GSL = as.integer(DATE_H - DATE_P))
hist_mean <- hist %>% 
  group_by(County) %>% 
  summarise(GSL_hist = mean(GSL, na.rm = TRUE), 
            .groups = "drop")

seasons <- expand_grid(Model = models, 
                       Scenario = scenarios) %>% 
  rowwise() %>% 
  mutate(data = list(read_csv(paste0("data/ISIMIP3a/adapted/distributions/", 
                                     Model, "/", Scenario, "/adapted_seasons.csv"), 
                              col_types = cols()))) %>% 
  ungroup() %>% 
  unnest(data) %>% 
  inner_join(hist_mean, by = "County") %>% 
  mutate(GSL_diff = GSL - GSL_hist)


ggplot(seasons, aes(x = Year, y = GSL)) + theme_classic() + 
  geom_line(aes(group = County), alpha = 0.4) + 
  facet_grid(Scenario ~ Model)

ggplot(seasons, aes(x = Year, y = GSL)) + theme_classic() + 
  geom_boxplot(aes(group = Year), outlier.shape = 1, outlier.size = 0.5, 
               outlier.colour = "red") + 
  facet_grid(Scenario ~ Model)

### Add historical season lengths for comparison

ggplot(seasons, aes(x = Year, y = GSL_diff)) + theme_classic() + 
  geom_boxplot(aes(group = Year), outlier.shape = 1, outlier.size = 0.5, 
               outlier.colour = "red") + 
  geom_hline(yintercept = 0, linetype = 2, colour = "green") + 
  facet_grid(Scenario ~ Model)


shift_sum <- seasons %>% 
  group_by(Model, Scenario, Year) %>% 
  summarise(GSL_diff = median(GSL_diff, na.rm = TRUE), 
            .groups = "drop")

ggplot(shift_sum) + theme_classic() + 
  geom_point(aes(x = Year, y = GSL_diff, colour = Model), size = 0.5, alpha = 0.8) + 
  geom_smooth(aes(x = Year, y = GSL_diff, colour = Model), method = "loess", 
              se = FALSE, size = 0.75) + 
  geom_hline(yintercept = 0, linetype = 2) + 
  facet_grid(. ~ Scenario) + 
  scale_colour_brewer(type = "qual", palette = "Set2") + 
  scale_y_continuous(breaks = seq(-21, 105, 7)) +
  labs(x = "", y = "Shift (d)", colour = "") + 
  theme(panel.grid.major.y = element_line(), 
        strip.text = element_text(face = "bold"))
ggsave("figures/adapted_gsl.png", width = 9, height = 4, units = "in")
