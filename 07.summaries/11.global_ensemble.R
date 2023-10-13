library(tidyverse)
source("src/cal_scales.R")


ref_cal <- read_rds("data/climate_summaries/reference_cal.rds")[[4]] %>% 
  mutate(Adaptation = "Reference")
plant_only <- read_rds("data/climate_summaries/plant_only.rds")[[4]] %>% 
  mutate(Adaptation = "Planting only")
plant_gdd <- read_rds("data/climate_summaries/plant_gdd.rds")[[4]] %>% 
  mutate(Adaptation = "Planting & maturity")
opt_cycle <- read_rds("data/climate_summaries/opt_cycle.rds")[[4]] %>% 
  mutate(Adaptation = "Optimal cycle")

dd <- bind_rows(ref_cal, plant_only, plant_gdd, opt_cycle) %>% 
  mutate(Adaptation = factor(Adaptation, levels = c("Reference", "Planting only", 
                                                    "Planting & maturity", 
                                                    "Optimal cycle"), 
                             ordered = TRUE))

png("figures/Fig_global_ensemble.png", width = 6, height = 3.5, 
    units = "in", res = 200)
ggplot(dd, aes(x = Scenario)) + theme_classic() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_linerange(aes(ymin = Lower, ymax = Upper, group = Adaptation, 
                     colour = Adaptation), position = position_dodge(0.3), 
                 size = 0.75) + 
  geom_point(aes(y = Mean, group = Adaptation, colour = Adaptation), 
             position = position_dodge(0.3), size = 2) + 
  facet_wrap(~ Period, ncol = 1, strip.position = "right") + 
  scale_y_continuous(labels = scales::label_percent(), limits = c(-1, 0)) + 
  scale_x_discrete(labels = c("SSP1-2.6", "SSP3-7.0", "SSP5-8.5")) + 
  scale_colour_manual(values = cal_sc) + 
  labs(x = "", y = "Mean relative yield", colour = "") + 
  theme(axis.title = element_text(face = "bold"), 
        strip.text = element_text(face = "bold"), 
        panel.grid.major.y = element_line(colour = "grey60", size = 0.25), 
        panel.grid.minor.y = element_line(colour = "grey80", size = 0.25))
dev.off()


ref_cal <- read_rds("data/climate_summaries/reference_cal.rds")[[2]] %>% 
  mutate(Adaptation = "Reference")
plant_only <- read_rds("data/climate_summaries/plant_only.rds")[[2]] %>% 
  mutate(Adaptation = "Planting only")
plant_gdd <- read_rds("data/climate_summaries/plant_gdd.rds")[[2]] %>% 
  mutate(Adaptation = "Planting & maturity")
opt_cycle <- read_rds("data/climate_summaries/opt_cycle.rds")[[2]] %>% 
  mutate(Adaptation = "Optimal cycle")

dd <- bind_rows(ref_cal, plant_only, plant_gdd, opt_cycle) %>% 
  mutate(Adaptation = factor(Adaptation, levels = c("Reference", "Planting only", 
                                                    "Planting & maturity", 
                                                    "Optimal cycle"), 
                             ordered = TRUE))

png("figures/FigS_global_models.png", width = 10, height = 5, 
    units = "in", res = 200)
ggplot(dd, aes(x = Model)) + theme_classic() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_linerange(aes(ymin = Lower, ymax = Upper, group = Adaptation, 
                     colour = Adaptation), position = position_dodge(0.7), 
                 size = 0.75) + 
  geom_point(aes(y = Mean, group = Adaptation, colour = Adaptation), 
             position = position_dodge(0.7), size = 2) + 
  facet_grid(Period ~ Scenario, labeller = labeller(Scenario = c("ssp126" = "SSP1-2.6", 
                                                                 "ssp370" = "SSP3-7.0", 
                                                                 "ssp585" = "SSP5-8.5"))) + 
  scale_y_continuous(labels = scales::label_percent(), limits = c(-1, 0.1)) + 
  scale_x_discrete(labels = model_lbls) +
  scale_colour_manual(values = cal_sc) + 
  labs(x = "", y = "Mean relative yield", colour = "") + 
  theme(axis.title = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.text = element_text(face = "bold"), 
        panel.grid.major.y = element_line(colour = "grey60", size = 0.25), 
        panel.grid.minor.y = element_line(colour = "grey80", size = 0.25))
dev.off()
