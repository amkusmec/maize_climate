library(tidyverse)
library(abind)
library(grid)
library(gridExtra)
source("src/bc_sum.R")
source("src/cal_scales.R")

ref_cal <- read_rds("data/climate_summaries/group_reference_cal.rds")[[2]] %>% 
  mutate(Adaptation = "Reference")
plant_only <- read_rds("data/climate_summaries/group_plant_only.rds")[[2]] %>% 
  mutate(Adaptation = "Planting only")
plant_gdd <- read_rds("data/climate_summaries/group_plant_gdd.rds")[[2]] %>% 
  mutate(Adaptation = "Planting & maturity")
opt_cycle <- read_rds("data/climate_summaries/group_opt_cycle.rds")[[2]] %>% 
  mutate(Adaptation = "Optimal cycle")

dd <- bind_rows(ref_cal, plant_only, plant_gdd, opt_cycle) %>% 
  mutate(Adaptation = factor(Adaptation, levels = c("Reference", "Planting only", 
                                                    "Planting & maturity", 
                                                    "Optimal cycle"), 
                             ordered = TRUE), 
         Group = recode_factor(Group, "Early" = "1934-1943", "Mid" = "1973-1984", 
                               "Late" = "2005-2014"))

ref_cal2 <- read_rds("data/climate_summaries/group_reference_cal.rds")[[1]]
plant_only2 <- read_rds("data/climate_summaries/group_plant_only.rds")[[1]]
plant_gdd2 <- read_rds("data/climate_summaries/group_plant_gdd.rds")[[1]]
opt_cycle2 <- read_rds("data/climate_summaries/group_opt_cycle.rds")[[1]]

names(ref_cal2) <- paste0("SSP", c("1-2.6", "3-7.0", "5-8.5"))

rc_wo <- map2_df(ref_cal2, paste0("SSP", c("1-2.6", "3-7.0", "5-8.5")), function(s, sc) {
  temp <- apply(s, 1, function(x) {
        apply(x < s[1, , , ], c(1, 3), mean)
      }, simplify = FALSE)
  map2_df(temp, names(temp), function(m, y) {
    as_tibble(t(m), rownames = "Period") %>% 
      mutate(Adaptation = "Reference", 
             Scenario = sc, 
             Period = c("2040-2069", "2070-2099"), 
             Group = y) %>% 
      pivot_longer(`gfdl-esm4`:`ukesm1-0-ll`, names_to = "Model", values_to = "Prob") %>% 
      select(Adaptation:Scenario, Model, Group, Model, Period, Prob)
  })
})
po_wo <- map2_df(plant_only2, paste0("SSP", c("1-2.6", "3-7.0", "5-8.5")), function(s, sc) {
  temp <- apply(s, 1, function(x) {
      apply(x < ref_cal2[[sc]][1, , , ], c(1, 3), mean)
    }, simplify = FALSE)
    map2_df(temp, names(temp), function(m, y) {
      as_tibble(t(m), rownames = "Period") %>% 
        mutate(Adaptation = "Planting only", 
               Scenario = sc, 
               Period = c("2040-2069", "2070-2099"), 
               Group = y) %>% 
        pivot_longer(`gfdl-esm4`:`ukesm1-0-ll`, names_to = "Model", values_to = "Prob") %>% 
        select(Adaptation:Scenario, Model, Group, Model, Period, Prob)
    })
})
pm_wo <- map2_df(plant_gdd2, paste0("SSP", c("1-2.6", "3-7.0", "5-8.5")), function(s, sc) {
  temp <- apply(s, 1, function(x) {
    apply(x < ref_cal2[[sc]][1, , , ], c(1, 3), mean)
  }, simplify = FALSE)
  map2_df(temp, names(temp), function(m, y) {
    as_tibble(t(m), rownames = "Period") %>% 
      mutate(Adaptation = "Planting & maturity", 
             Scenario = sc, 
             Period = c("2040-2069", "2070-2099"), 
             Group = y) %>% 
      pivot_longer(`gfdl-esm4`:`ukesm1-0-ll`, names_to = "Model", values_to = "Prob") %>% 
      select(Adaptation:Scenario, Model, Group, Model, Period, Prob)
  })
})
oc_wo <- map2_df(opt_cycle2, paste0("SSP", c("1-2.6", "3-7.0", "5-8.5")), function(s, sc) {
  temp <- apply(s, 1, function(x) {
    apply(x < ref_cal2[[sc]][1, , , ], c(1, 3), mean)
  }, simplify = FALSE)
  map2_df(temp, names(temp), function(m, y) {
    as_tibble(t(m), rownames = "Period") %>% 
      mutate(Adaptation = "Optimal cycle", 
             Scenario = sc, 
             Period = c("2040-2069", "2070-2099"), 
             Group = y) %>% 
      pivot_longer(`gfdl-esm4`:`ukesm1-0-ll`, names_to = "Model", values_to = "Prob") %>% 
      select(Adaptation:Scenario, Model, Group, Model, Period, Prob)
  })
})

dd_wo <- bind_rows(rc_wo, po_wo, pm_wo, oc_wo) %>% 
  mutate(Adaptation = factor(Adaptation, levels = c("Reference", "Planting only", 
                                                    "Planting & maturity", "Optimal cycle"), 
                             ordered = TRUE), 
         Group = recode_factor(Group, "Early" = "1934-1943", "Mid" = "1973-1984", 
                               "Late" = "2005-2014"), 
         Scenario = factor(Scenario, levels = c("SSP1-2.6", "SSP3-7.0", "SSP5-8.5"), 
                           ordered = TRUE), 
         AP = interaction(Adaptation, Period))




p1a <- ggplot(filter(dd, Scenario == "ssp126"), aes(x = Adaptation)) + theme_classic() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_linerange(aes(ymin = Lower, ymax = Upper, group = Group, 
                     colour = Group), position = position_dodge(0.7), 
                 size = 0.75) + 
  geom_point(aes(y = Mean, group = Group, colour = Group), 
             position = position_dodge(0.7), size = 2) + 
  facet_grid(Period ~ Model, labeller = labeller(Model = as_labeller(model_lbls, default = label_parsed))) + 
  scale_y_continuous(labels = scales::label_percent(), limits = c(-1, 0.15)) + 
  scale_colour_manual(values = el) +
  labs(x = "", y = "Mean relative yield", colour = "", tag = "A", subtitle = "SSP1-2.6") + 
  theme(axis.title = element_text(face = "bold"), 
        strip.text = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.grid.major.y = element_line(colour = "grey60", size = 0.25), 
        panel.grid.minor.y = element_line(colour = "grey80", size = 0.25))


p1b <- ggplot(filter(dd_wo, Scenario == "SSP1-2.6"), aes(x = Group, y = Adaptation)) + theme_classic() + 
  geom_tile(aes(fill = Prob), colour = "black") + 
  facet_grid(Period ~ Model, labeller = labeller(Model = as_labeller(model_lbls, default = label_parsed))) + 
  scale_fill_steps2(show.limits = TRUE, labels = scales::label_percent(), 
                    midpoint = 0.5, low = scales::muted("blue"), high = scales::muted("red"), 
                    n.breaks = 6) + 
  labs(x = "", y = "", fill = "Probability", tag = "B", subtitle = "") + 
  theme(strip.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1))

gp1 <- arrangeGrob(ggplotGrob(p1a), ggplotGrob(p1b), 
                   layout_matrix = matrix(1:2, ncol = 1))
png("figures/FigS_group_ssp126.png", width = 9, height = 7, 
    units = "in", res = 200)
plot(gp1)
dev.off()

p3a <- ggplot(filter(dd, Scenario == "ssp370"), aes(x = Adaptation)) + theme_classic() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_linerange(aes(ymin = Lower, ymax = Upper, group = Group, 
                     colour = Group), position = position_dodge(0.7), 
                 size = 0.75) + 
  geom_point(aes(y = Mean, group = Group, colour = Group), 
             position = position_dodge(0.7), size = 2) + 
  facet_grid(Period ~ Model, labeller = labeller(Model = as_labeller(model_lbls, default = label_parsed))) + 
  scale_y_continuous(labels = scales::label_percent(), limits = c(-1, 0.15)) + 
  scale_colour_manual(values = el) +
  labs(x = "", y = "Mean relative yield", colour = "", tag = "A", subtitle = "SSP3-7.0") + 
  theme(axis.title = element_text(face = "bold"), 
        strip.text = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.grid.major.y = element_line(colour = "grey60", size = 0.25), 
        panel.grid.minor.y = element_line(colour = "grey80", size = 0.25))


p3b <- ggplot(filter(dd_wo, Scenario == "SSP3-7.0"), aes(x = Group, y = Adaptation)) + theme_classic() + 
  geom_tile(aes(fill = Prob), colour = "black") + 
  facet_grid(Period ~ Model, labeller = labeller(Model = as_labeller(model_lbls, default = label_parsed))) + 
  scale_fill_steps2(show.limits = TRUE, labels = scales::label_percent(), 
                    midpoint = 0.5, low = scales::muted("blue"), high = scales::muted("red"), 
                    n.breaks = 6) + 
  labs(x = "", y = "", fill = "Probability", tag = "B", subtitle = "") + 
  theme(strip.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1))

gp3 <- arrangeGrob(ggplotGrob(p3a), ggplotGrob(p3b), 
                   layout_matrix = matrix(1:2, ncol = 1))
png("figures/FigS_group_ssp370.png", width = 9, height = 7, 
    units = "in", res = 200)
plot(gp3)
dev.off()

p5a <- ggplot(filter(dd, Scenario == "ssp585"), aes(x = Adaptation)) + theme_classic() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_linerange(aes(ymin = Lower, ymax = Upper, group = Group, 
                     colour = Group), position = position_dodge(0.7), 
                 size = 0.75) + 
  geom_point(aes(y = Mean, group = Group, colour = Group), 
             position = position_dodge(0.7), size = 2) + 
  facet_grid(Period ~ Model, labeller = labeller(Model = as_labeller(model_lbls, default = label_parsed))) + 
  scale_y_continuous(labels = scales::label_percent(), limits = c(-1, 0.15)) + 
  scale_colour_manual(values = el) +
  labs(x = "", y = "Mean relative yield", colour = "", tag = "A", subtitle = "SSP5-8.5") + 
  theme(axis.title = element_text(face = "bold"), 
        strip.text = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.grid.major.y = element_line(colour = "grey60", size = 0.25), 
        panel.grid.minor.y = element_line(colour = "grey80", size = 0.25))


p5b <- ggplot(filter(dd_wo, Scenario == "SSP5-8.5"), aes(x = Group, y = Adaptation)) + theme_classic() + 
  geom_tile(aes(fill = Prob), colour = "black") + 
  facet_grid(Period ~ Model, labeller = labeller(Model = as_labeller(model_lbls, default = label_parsed))) + 
  scale_fill_steps2(show.limits = TRUE, labels = scales::label_percent(), 
                    midpoint = 0.5, low = scales::muted("blue"), high = scales::muted("red"), 
                    n.breaks = 6) + 
  labs(x = "", y = "", fill = "Probability", tag = "B", subtitle = "") + 
  theme(strip.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1))

gp5 <- arrangeGrob(ggplotGrob(p5a), ggplotGrob(p5b), 
                   layout_matrix = matrix(1:2, ncol = 1))
png("figures/FigS_group_ssp585.png", width = 9, height = 7, 
    units = "in", res = 200)
plot(gp5)
dev.off()
