library(tidyverse)
library(grid)
library(gridExtra)

# Bounds on FF are 60 (1 March) and 334 (30 November)
gcm <- read_csv("data/gcm_frost_free.csv", col_types = cols()) %>%
  mutate(FFL = End - Start)

models_ensemble <- gcm %>% 
  filter(!if_any(c(Start:End, FFL), ~ is.infinite(.x))) %>% 
  group_by(Scenario, County, YEAR) %>% 
  summarise(across(c(Start:End, FFL), ~ mean(.x)), 
            .groups = "drop") %>% 
  nest(data = c(YEAR:End, FFL)) %>% 
  rowwise() %>% 
  mutate(Model_Start = list(lm(Start ~ 1 + YEAR, data = data)), 
         Model_End = list(lm(End ~ 1 + YEAR, data = data)), 
         Model_FFL = list(lm(FFL ~ 1 + YEAR, data = data))) %>% 
  ungroup() %>% 
  mutate(across(Model_Start:Model_FFL, 
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
  filter(COUNTYNS %in% gcm$County) %>% 
  mutate(NAME = str_to_upper(NAME) %>% 
           str_replace("DEKALB", "DE KALB") %>% 
           str_replace("O'BRIEN", "OBRIEN") %>% 
           str_replace("ST. CLAIR", "ST CLAIR"))
county_df <- map_data("county") %>% 
  filter(region %in% state_names) %>% 
  mutate(subregion = str_to_upper(subregion)) %>% 
  filter(subregion %in% counties$NAME) %>% 
  inner_join(select(counties, NAME, COUNTYNS), by = c("subregion" = "NAME"))


df_slope <- left_join(county_df, models_ensemble, by = c("COUNTYNS" = "County")) %>% 
  select(long:geometry, Scenario, contains("Slope")) %>% 
  pivot_longer(contains("Slope"), names_to = "Variable", values_to = "Value") %>% 
  mutate(Variable = str_remove(Variable, "Model_") %>% str_remove("_Slope") %>% 
           factor(levels = c("Start", "End", "FFL"), ordered = TRUE))
pA1 <- ggplot() + theme_classic() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Value),
               data = filter(df_slope, Scenario == "ssp126"), colour = "black", size = 0.25) +
  geom_polygon(aes(x = long, y = lat, group = region), data = states, 
               colour = "black", fill = "white", size = 0.75, alpha = 0) + 
  scale_fill_steps2(na.value = "transparent", show.limits = TRUE, n.breaks = 6) + 
  scale_x_continuous(breaks = seq(-85, -105, -2.5), 
                     labels = parse(text = paste0(seq(85, 105, 2.5), "*degree", "*W"))) + 
  scale_y_continuous(breaks = seq(36, 47, 2), 
                     labels = parse(text = paste0(seq(36, 47, 2), "*degree", "*N"))) + 
  facet_wrap(~ Variable, ncol = 1, strip.position = "left") + 
  labs(x = "", y = "", fill = "d/yr", tag = "A", subtitle = "SSP1-2.6") + 
  theme(legend.title = element_text(face = "bold"), 
        legend.position = "left", 
        strip.text = element_text(face = "bold"))
pB1 <- ggplot() + theme_classic() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Value),
               data = filter(df_slope, Scenario == "ssp370"), colour = "black", size = 0.25) +
  geom_polygon(aes(x = long, y = lat, group = region), data = states, 
               colour = "black", fill = "white", size = 0.75, alpha = 0) + 
  scale_fill_steps2(na.value = "transparent", show.limits = TRUE, n.breaks = 6) + 
  scale_x_continuous(breaks = seq(-85, -105, -2.5), 
                     labels = parse(text = paste0(seq(85, 105, 2.5), "*degree", "*W"))) + 
  scale_y_continuous(breaks = seq(36, 47, 2), 
                     labels = parse(text = paste0(seq(36, 47, 2), "*degree", "*N"))) + 
  facet_wrap(~ Variable, ncol = 1, strip.position = "left") + 
  labs(x = "", y = "", fill = "d/yr", tag = "B", subtitle = "SSP3-7.0") + 
  theme(legend.title = element_text(face = "bold"), 
        legend.position = "left", 
        strip.text = element_text(face = "bold"))
pC1 <- ggplot() + theme_classic() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Value),
               data = filter(df_slope, Scenario == "ssp585"), colour = "black", size = 0.25) +
  geom_polygon(aes(x = long, y = lat, group = region), data = states, 
               colour = "black", fill = "white", size = 0.75, alpha = 0) + 
  scale_fill_steps2(na.value = "transparent", show.limits = TRUE, n.breaks = 6) + 
  scale_x_continuous(breaks = seq(-85, -105, -2.5), 
                     labels = parse(text = paste0(seq(85, 105, 2.5), "*degree", "*W"))) + 
  scale_y_continuous(breaks = seq(36, 47, 2), 
                     labels = parse(text = paste0(seq(36, 47, 2), "*degree", "*N"))) + 
  facet_wrap(~ Variable, ncol = 1, strip.position = "left") + 
  labs(x = "", y = "", fill = "d/yr", tag = "C", subtitle = "SSP5-8.5") + 
  theme(legend.title = element_text(face = "bold"), 
        legend.position = "left", 
        strip.text = element_text(face = "bold"))


df_pval <- left_join(county_df, models_ensemble, by = c("COUNTYNS" = "County")) %>% 
  select(long:geometry, Scenario, contains("Pval")) %>% 
  pivot_longer(contains("Pval"), names_to = "Variable", values_to = "Value") %>% 
  mutate(Variable = str_remove(Variable, "Model_") %>% str_remove("_Pval") %>% 
           factor(levels = c("Start", "End", "FFL"), ordered = TRUE), 
         Value = case_when(
           Value <= 0.01 ~ "<= 0.01", 
           Value <= 0.05 ~ "<= 0.05", 
           Value <= 0.1 ~ "<= 0.1", 
           TRUE ~ "ns"
         ) %>% 
           factor(levels = c("ns", "<= 0.1", "<= 0.05", "<= 0.01"), ordered = TRUE)
  )
pA2 <- ggplot() + theme_classic() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Value),
               data = filter(df_pval, Scenario == "ssp126"), colour = "black", size = 0.25) +
  geom_polygon(aes(x = long, y = lat, group = region), data = states, 
               colour = "black", fill = "white", size = 0.75, alpha = 0) + 
  scale_x_continuous(breaks = seq(-85, -105, -2.5), 
                     labels = parse(text = paste0(seq(85, 105, 2.5), "*degree", "*W"))) + 
  scale_y_continuous(breaks = seq(36, 47, 2), 
                     labels = parse(text = paste0(seq(36, 47, 2), "*degree", "*N"))) + 
  scale_fill_brewer(type = "seq", palette = "YlOrRd", 
                    labels = parse(text = c("ns", "p<=0.1", "p<=0.05", "p<=0.01"))) + 
  facet_wrap(~ Variable, ncol = 1, strip.position = "right") + 
  labs(x = "", y = "", fill = "", tag = "", subtitle = "") + 
  theme(legend.title = element_text(face = "bold"), 
        legend.position = "right", 
        strip.text = element_text(face = "bold"))
pB2 <- ggplot() + theme_classic() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Value),
               data = filter(df_pval, Scenario == "ssp370"), colour = "black", size = 0.25) +
  geom_polygon(aes(x = long, y = lat, group = region), data = states, 
               colour = "black", fill = "white", size = 0.75, alpha = 0) + 
  scale_x_continuous(breaks = seq(-85, -105, -2.5), 
                     labels = parse(text = paste0(seq(85, 105, 2.5), "*degree", "*W"))) + 
  scale_y_continuous(breaks = seq(36, 47, 2), 
                     labels = parse(text = paste0(seq(36, 47, 2), "*degree", "*N"))) + 
  scale_fill_manual(values = c("#e31a1c"), labels = parse(text = c("p<=0.01"))) + 
  facet_wrap(~ Variable, ncol = 1, strip.position = "right") + 
  labs(x = "", y = "", fill = "", tag = "", subtitle = "") + 
  theme(legend.title = element_text(face = "bold"), 
        legend.position = "right", 
        strip.text = element_text(face = "bold"))
pC2 <- ggplot() + theme_classic() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Value),
               data = filter(df_pval, Scenario == "ssp585"), colour = "black", size = 0.25) +
  geom_polygon(aes(x = long, y = lat, group = region), data = states, 
               colour = "black", fill = "white", size = 0.75, alpha = 0) + 
  scale_x_continuous(breaks = seq(-85, -105, -2.5), 
                     labels = parse(text = paste0(seq(85, 105, 2.5), "*degree", "*W"))) + 
  scale_y_continuous(breaks = seq(36, 47, 2), 
                     labels = parse(text = paste0(seq(36, 47, 2), "*degree", "*N"))) + 
  scale_fill_manual(values = c("#e31a1c"), labels = parse(text = c("p<=0.01"))) + 
  facet_wrap(~ Variable, ncol = 1, strip.position = "right") + 
  labs(x = "", y = "", fill = "", tag = "", subtitle = "") + 
  theme(legend.title = element_text(face = "bold"), 
        legend.position = "right", 
        strip.text = element_text(face = "bold"))

gp <- arrangeGrob(ggplotGrob(pA1), ggplotGrob(pA2), ggplotGrob(pB1), 
                  ggplotGrob(pB2), ggplotGrob(pC1), ggplotGrob(pC2), 
                  layout_matrix = matrix(1:8, nrow = 2, byrow = TRUE))
ggsave("figures/Fig_ff_trends_gcm.png", gp, width = 20, height = 10, units = "in")
