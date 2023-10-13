library(tidyverse)
library(grid)
library(gridExtra)
source("src/bc_sum.R")
source("src/cal_scales.R")


hyb_counts <- read_rds("data/ISIMIP3a/hybrid_counts.rds")$group

beta <- read_rds("data/historical/CUB_temperature_fixed.rds")
allBetaCurves <- read_rds("data/historical/CUB_temperature_random.rds")

idx <- which(dimnames(allBetaCurves)[[2]] %in% hyb_counts$Variety)
allBetaCurves <- allBetaCurves[, idx, ]
gc()

for (i in 1:(dim(allBetaCurves)[2])) {
  allBetaCurves[-1, i, ] <- allBetaCurves[-1, i, ] + beta
}

gr_lvl <- hyb_counts$Group %>% unique() %>% sort()
group_fn <- apply(allBetaCurves, c(1, 3), function(x) {
  sapply(gr_lvl, function(g) weighted.mean(x[hyb_counts$Group == g], hyb_counts$Wt[hyb_counts$Group == g], na.rm = TRUE))
})

group_post <- map2_df(1:(dim(group_fn)[1]), dimnames(group_fn)[[1]], 
                      function(i, g) {
                        apply(group_fn[i, , ], 1, bc_sum) %>% 
                          t() %>% 
                          as_tibble() %>% 
                          mutate(Group = g, 
                                 Degree = -2:41) %>% 
                          select(Group:Degree, everything())
                      }) %>% 
  filter(Degree >= 0) %>% 
  mutate(Group = recode_factor(Group, "Early" = "1934-1943", "Mid" = "1973-1984", 
                               "Late" = "2005-2014"))

pA <- ggplot(group_post) + theme_bw() + 
  geom_line(aes(x = Degree, y = Mean, colour = Group), size = 0.75) + 
  geom_line(aes(x = Degree, y = Lower, colour = Group), size = 0.5, linetype = 2) +
  geom_line(aes(x = Degree, y = Upper, colour = Group), size = 0.5, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2, colour = "grey60", size = 0.75) + 
  geom_vline(xintercept = c(8, 30, 37), linetype = 2, colour = "grey60", size = 0.75) + 
  scale_colour_manual(values = el) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) + 
  labs(x = expression(bold("Temperature ("*degree*"C)")), 
       y = "% yield/h exposure", tag = "A") + 
  annotate("text", x = c(20, 33.5, 40), y = -0.0175, fontface = "bold", size = 3, 
           label = c("Optimal", "Moderate\nstress", "Severe\nstress")) + 
  theme(legend.title = element_blank(), 
        legend.background = element_rect(fill = "transparent"), 
        legend.position = c(0.125, 0.2), 
        axis.title = element_text(face = "bold"))



ref_cal <- read_rds("data/climate_summaries/group_reference_cal.rds")[[4]] %>% 
  mutate(Adaptation = "Reference")
plant_only <- read_rds("data/climate_summaries/group_plant_only.rds")[[4]] %>% 
  mutate(Adaptation = "Planting only")
plant_gdd <- read_rds("data/climate_summaries/group_plant_gdd.rds")[[4]] %>% 
  mutate(Adaptation = "Planting & maturity")
opt_cycle <- read_rds("data/climate_summaries/group_opt_cycle.rds")[[4]] %>% 
  mutate(Adaptation = "Optimal cycle")

dd <- bind_rows(ref_cal, plant_only, plant_gdd, opt_cycle) %>% 
  mutate(Adaptation = factor(Adaptation, levels = c("Reference", "Planting only", 
                                                    "Planting & maturity", 
                                                    "Optimal cycle"), 
                             ordered = TRUE), 
         Group = recode_factor(Group, "Early" = "1934-1943", "Mid" = "1973-1984", 
                               "Late" = "2005-2014"))

pB <- ggplot(dd, aes(x = Adaptation)) + theme_classic() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_linerange(aes(ymin = Lower, ymax = Upper, group = Group, 
                     colour = Group), position = position_dodge(0.7), 
                 size = 0.75) + 
  geom_point(aes(y = Mean, group = Group, colour = Group), 
             position = position_dodge(0.7), size = 2) + 
  facet_grid(Period ~ Scenario, 
             labeller = labeller(Scenario = as_labeller(c("ssp126" = "SSP1-2.6", 
                                                          "ssp370" = "SSP3-7.0", 
                                                          "ssp585" = "SSP5-8.5")))) + 
  scale_y_continuous(labels = scales::label_percent(), limits = c(-1, 0.15)) + 
  scale_colour_manual(values = el) +
  labs(x = "", y = "Mean relative yield", colour = "", tag = "B") + 
  guides(colour = "none") + 
  theme(axis.title = element_text(face = "bold"), 
        strip.text = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.grid.major.y = element_line(colour = "grey60", size = 0.25), 
        panel.grid.minor.y = element_line(colour = "grey80", size = 0.25))


ref_cal2 <- read_rds("data/climate_summaries/group_reference_cal.rds")[[3]]
plant_only2 <- read_rds("data/climate_summaries/group_plant_only.rds")[[3]]
plant_gdd2 <- read_rds("data/climate_summaries/group_plant_gdd.rds")[[3]]
opt_cycle2 <- read_rds("data/climate_summaries/group_opt_cycle.rds")[[3]]

names(ref_cal2) <- paste0("SSP", c("1-2.6", "3-7.0", "5-8.5"))

rc_wo <- map2_df(ref_cal2, paste0("SSP", c("1-2.6", "3-7.0", "5-8.5")), function(s, sc) {
  apply(s, 1, function(x) colMeans(x < s[1, , ])) %>% 
    as_tibble() %>% 
    mutate(Period = c("2040-2069", "2070-2099")) %>% 
    pivot_longer(-Period, names_to = "Group", values_to = "Prob") %>% 
    mutate(Adaptation = "Reference calendar", 
           Scenario = sc) %>% 
    select(Adaptation:Scenario, Group, Period, Prob)
})
po_wo <- map2_df(plant_only2, paste0("SSP", c("1-2.6", "3-7.0", "5-8.5")), function(s, sc) {
  apply(s, 1, function(x) colMeans(x < ref_cal2[[sc]][1, , ])) %>% 
    as_tibble() %>% 
    mutate(Period = c("2040-2069", "2070-2099")) %>% 
    pivot_longer(-Period, names_to = "Group", values_to = "Prob") %>% 
    mutate(Adaptation = "Planting only", 
           Scenario = sc) %>% 
    select(Adaptation:Scenario, Group, Period, Prob)
})
pm_wo <- map2_df(plant_gdd2, paste0("SSP", c("1-2.6", "3-7.0", "5-8.5")), function(s, sc) {
  apply(s, 1, function(x) colMeans(x < ref_cal2[[sc]][1, , ])) %>% 
    as_tibble() %>% 
    mutate(Period = c("2040-2069", "2070-2099")) %>% 
    pivot_longer(-Period, names_to = "Group", values_to = "Prob") %>% 
    mutate(Adaptation = "Planting & maturity", 
           Scenario = sc) %>% 
    select(Adaptation:Scenario, Group, Period, Prob)
})
oc_wo <- map2_df(opt_cycle2, paste0("SSP", c("1-2.6", "3-7.0", "5-8.5")), function(s, sc) {
  apply(s, 1, function(x) colMeans(x < ref_cal2[[sc]][1, , ])) %>% 
    as_tibble() %>% 
    mutate(Period = c("2040-2069", "2070-2099")) %>% 
    pivot_longer(-Period, names_to = "Group", values_to = "Prob") %>% 
    mutate(Adaptation = "Optimal cycle", 
           Scenario = sc) %>% 
    select(Adaptation:Scenario, Group, Period, Prob)
})

dd_wo <- bind_rows(rc_wo, po_wo, pm_wo, oc_wo) %>% 
  mutate(Adaptation = factor(Adaptation, levels = c("Reference calendar", "Planting only", 
                                                    "Planting & maturity", "Optimal cycle"), 
                             ordered = TRUE), 
         Group = recode_factor(Group, "Early" = "1934-1943", "Mid" = "1973-1984", 
                               "Late" = "2005-2014"), 
         Scenario = factor(Scenario, levels = c("SSP1-2.6", "SSP3-7.0", "SSP5-8.5"), 
                           ordered = TRUE), 
         AP = interaction(Adaptation, Period))

pC <- ggplot(dd_wo, aes(x = Group, y = Adaptation)) + theme_classic() + 
  geom_tile(aes(fill = Prob), colour = "black") + 
  facet_grid(Period ~ Scenario) + 
  scale_fill_steps2(show.limits = TRUE, labels = scales::label_percent(), 
                    midpoint = 0.5, low = scales::muted("blue"), high = scales::muted("red"), 
                    n.breaks = 7) + 
  labs(x = "", y = "", fill = "Probability", tag = "C") + 
  theme(strip.text = element_text(face = "bold"), 
        legend.title = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1))

gp <- arrangeGrob(ggplotGrob(pA), ggplotGrob(pB), ggplotGrob(pC), 
                  layout_matrix = matrix(c(1:3, 2), nrow = 2, byrow = TRUE))
png("figures/Fig_group_ensemble_all.png", width = 11, height = 6, 
    units = "in", res = 200)
plot(gp)
dev.off()
