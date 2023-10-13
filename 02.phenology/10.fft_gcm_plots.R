library(tidyverse)
library(grid)
library(gridExtra)


fft <- read_csv("data/fft_gcm.csv") %>% 
  group_by(Scenario, County, YEAR) %>% 
  summarise(across(c(GDD:HSDD2), ~ mean(.x)), 
            .groups = "drop") %>% 
  nest(data = c(YEAR:HSDD2)) %>% 
  rowwise() %>% 
  mutate(Model_GDD = list(lm(GDD ~ 1 + YEAR, data = mutate(data, YEAR = YEAR - min(YEAR)))), 
         Model_HSDD = list(lm(HSDD ~ 1 + YEAR, data = mutate(data, YEAR = YEAR - min(YEAR)))), 
         Model_HSDD1 = list(lm(HSDD1 ~ 1 + YEAR, data = mutate(data, YEAR = YEAR - min(YEAR)))), 
         Model_HSDD2 = list(lm(HSDD2 ~ 1 + YEAR, data = mutate(data, YEAR = YEAR - min(YEAR))))) %>% 
  ungroup() %>% 
  mutate(across(Model_GDD:Model_HSDD2, 
                .fns = list(
                    "Slope" = ~ map_dbl(.x, function(x) coef(x)[2] %>% unname()), 
                    "Pval" = ~ map_dbl(.x, function(x) summary(x)$coefficients[2, 4] %>% unname())
                  ), 
                .names = "{.col}_{.fn}"))

# State outlines
state_names <- c("illinois", "iowa", "nebraska", "kansas")
states <- map_data("state", region = state_names)

# Pair county outlines with slopes
counties <- sf::read_sf("data/prism/county_outlines/tl_2017_us_county.shp") %>% 
  filter(COUNTYNS %in% fft$County) %>% 
  mutate(NAME = str_to_upper(NAME) %>% 
           str_replace("DEKALB", "DE KALB") %>% 
           str_replace("O'BRIEN", "OBRIEN") %>% 
           str_replace("ST. CLAIR", "ST CLAIR"))
county_df <- map_data("county") %>% 
  filter(region %in% state_names) %>% 
  mutate(subregion = str_to_upper(subregion)) %>% 
  filter(subregion %in% counties$NAME) %>% 
  inner_join(select(counties, NAME, COUNTYNS), by = c("subregion" = "NAME"))


df_slope <- left_join(county_df, fft, by = c("COUNTYNS" = "County")) %>% 
  select(long:geometry, Scenario, contains("Slope")) %>% 
  pivot_longer(contains("Slope"), names_to = "Variable", values_to = "Value") %>% 
  mutate(Variable = str_remove(Variable, "Model_") %>% str_remove("_Slope")) %>% 
  filter(Variable %in% c("GDD", "HSDD")) %>%
  mutate(Variable = factor(Variable, levels = c("GDD", "HSDD"), ordered = TRUE))
df_pval <- left_join(county_df, fft, by = c("COUNTYNS" = "County")) %>% 
  select(long:geometry, Scenario, contains("Pval")) %>% 
  pivot_longer(contains("Pval"), names_to = "Variable", values_to = "Value") %>% 
  mutate(Variable = str_remove(Variable, "Model_") %>% str_remove("_Pval")) %>% 
  filter(Variable %in% c("GDD", "HSDD")) %>%
  mutate(Value = case_when(
            Value <= 0.01 ~ "<= 0.01", 
            Value <= 0.05 ~ "<= 0.05", 
            Value <= 0.1 ~ "<= 0.1", 
            TRUE ~ "ns"
          ) %>% 
        factor(levels = c("ns", "<= 0.1", "<= 0.05", "<= 0.01"), ordered = TRUE), 
      Variable = factor(Variable, levels = c("GDD", "HSDD"), ordered = TRUE)
    )

count(df_pval, Scenario, Variable, Value)

ggplot() + theme_classic() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Value),
               data = df_slope, colour = "black", size = 0.25) +
  geom_polygon(aes(x = long, y = lat, group = region), data = states, 
               colour = "black", fill = "white", size = 0.75, alpha = 0) + 
  scale_fill_steps(na.value = "transparent", show.limits = TRUE, n.breaks = 6, 
                   low = "#fee5d9", high = "#a50f15") + 
  scale_x_continuous(breaks = seq(-85, -105, -2.5), 
                     labels = parse(text = paste0(seq(85, 105, 2.5), "*degree", "*W"))) + 
  scale_y_continuous(breaks = seq(36, 47, 2), 
                     labels = parse(text = paste0(seq(36, 47, 2), "*degree", "*N"))) + 
  facet_grid(Scenario ~ Variable, 
             labeller = labeller(Scenario = as_labeller(c("ssp126" = "SSP1-2.6", 
                                                          "ssp370" = "SSP3-7.0", 
                                                          "ssp585" = "SSP5-8.5")))) + 
  labs(x = "", y = "", fill = expression(bold(degree*"C d/yr"))) + 
  theme(legend.title = element_text(face = "bold"), 
        legend.position = "left", 
        strip.text = element_text(face = "bold"))
ggsave("figures/Fig_fft_gcm_trends.png", width = 11, height = 7, units = "in")
