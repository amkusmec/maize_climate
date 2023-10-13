library(tidyverse)
library(grid)
library(gridExtra)
source("src/cal_scales.R")


models <- list.dirs("data/ISIMIP3a/distributions", 
                    full.names = FALSE, recursive = FALSE)
scenarios <- list.dirs(paste0("data/ISIMIP3a/distributions/", models[1]), 
                       full.names = FALSE, recursive = FALSE)[-1]

hist <- c("distributions/", "adapted/distributions/", "cycle_adapted/distributions/", 
          "reference_cal/distributions/") %>% 
    map_df(function(dd) {
      map_df(models, function(m) {
        paste0("data/ISIMIP3a/", dd, m, "/historical") %>% 
          list.files("*_temp\\.csv", full.names = TRUE) %>% 
          map_df(function(f) {
            read_csv(f, col_types = cols()) %>% 
              filter(YEAR >= 1983, YEAR <= 2013) %>% 
              filter(Degree >= 8) %>% 
              mutate(Stress = case_when(
                Degree >= 8 & Degree < 30 ~ "Optimal", 
                Degree >= 30 & Degree <= 37 ~ "Moderate", 
                Degree > 37 ~ "Severe"
              )) %>% 
              group_by(County, YEAR, Stress) %>% 
              summarise(Time = sum(Time), 
                        .groups = "drop") %>% 
              mutate(Model = m, 
                     Adaptation = dd)
          })
      })
    })

ssp <- c("distributions/", "adapted/distributions/", "cycle_adapted/distributions/", 
         "reference_cal/distributions/") %>% 
    map_df(function(dd) {
      map_df(models, function(m) { 
        map_df(scenarios, function(s) {
          paste0("data/ISIMIP3a/", dd, m, "/", s) %>% 
            list.files("*_temp\\.csv", full.names = TRUE) %>% 
            map_df(function(f) {
              read_csv(f, col_types = cols()) %>% 
                filter(YEAR <= 2099) %>% 
                filter(Degree >= 8) %>% 
                mutate(Stress = case_when(
                  Degree >= 8 & Degree < 30 ~ "Optimal", 
                  Degree >= 30 & Degree <= 37 ~ "Moderate", 
                  Degree > 37 ~ "Severe"
                )) %>% 
                group_by(County, YEAR, Stress) %>% 
                summarise(Time = sum(Time), 
                          .groups = "drop") %>% 
                mutate(Model = m, 
                       Scenario = s, 
                       Adaptation = dd)
            })
        })
      })
    })

### Against respective historical reference periods
hist_ref <- hist %>% 
  group_by(Adaptation, Stress, YEAR, County) %>%
  summarise(Time = mean(Time),
            .groups = "drop_last") %>%
  summarise(Time = mean(Time),
            .groups = "drop_last") %>%
  summarise(Time = mean(Time),
            .groups = "drop")
ssp_ref <- ssp %>% 
  group_by(Adaptation, Scenario, Stress, YEAR, County) %>%
  summarise(Time = mean(Time),
            .groups = "drop_last") %>%
  summarise(Time = mean(Time),
            .groups = "drop")

ff_ref <- inner_join(ssp_ref, hist_ref, by = c("Adaptation", "Stress")) %>% 
  mutate(FF = Time.x/Time.y, 
         Stress = factor(Stress, levels = c("Optimal", "Moderate", "Severe"), ordered = TRUE)) %>% 
  select(Adaptation:YEAR, FF) %>% 
  mutate(Adaptation = recode_factor(Adaptation, "reference_cal/distributions/" = "Reference", 
                                    "distributions/" = "Planting only", 
                                    "adapted/distributions/" = "Planting & maturity", 
                                    "cycle_adapted/distributions/" = "Optimal cycle") %>% 
           factor(labels = names(cal_sc)))

grp_lbls <- function(y) {
  sapply(y, function(x) {
    if (x == "Optimal") {
      expression(bold(paste(group("[",list(8,30),")")*degree*"C", sep = "")))
    } else if (x == "Moderate") {
      expression(bold(paste(group("[",list(30,37),"]")*degree*"C", sep = "")))
    } else {
      expression(bold(paste(group("[",list(38,infinity),")")*degree*"C", sep = "")))
    }
  })
}

pA <- ggplot(ff_ref, aes(x = YEAR, y = FF)) + theme_classic() + 
  geom_line(aes(colour = Adaptation), size = 0.75) + 
  geom_hline(yintercept = 1, linetype = 2) + 
  facet_grid(Stress ~ Scenario, scales = "free_y", 
             labeller = labeller(Scenario = c("ssp126" = "SSP1-2.6", 
                                              "ssp370" = "SSP3-7.0", 
                                              "ssp585" = "SSP5-8.5"), 
                                 Stress = as_labeller(grp_lbls, default = label_parsed))) + 
  scale_colour_manual(values = cal_sc) + 
  labs(x = "", y = "Exposure fold-change\nvs. 1983-2013", colour = "", tag = "B") + 
  theme(axis.title = element_text(face = "bold"), 
        strip.text = element_text(face = "bold"), 
        legend.direction = "horizontal", 
        legend.position = "bottom")

### Against reference calendar year-by-year
ref <- ssp %>% 
  filter(Adaptation == "reference_cal/distributions/") %>% 
  group_by(Scenario, Stress, YEAR, County) %>%
  summarise(Time = mean(Time),
            .groups = "drop_last") %>%
  summarise(Time = mean(Time),
            .groups = "drop")
ssp2 <- ssp %>% 
  filter(Adaptation != "reference_cal/distributions/") %>% 
  group_by(Adaptation, Scenario, Stress, YEAR, County) %>%
  summarise(Time = mean(Time),
            .groups = "drop_last") %>%
  summarise(Time = mean(Time),
            .groups = "drop") %>% 
  inner_join(ref, by = c("YEAR", "Stress", "Scenario")) %>% 
  mutate(FF = Time.x/Time.y, 
         Stress = factor(Stress, levels = c("Optimal", "Moderate", "Severe"), ordered = TRUE)) %>% 
  select(Adaptation:YEAR, Stress, FF) %>% 
  mutate(Adaptation = recode_factor(Adaptation, "reference_cal/distributions/" = "Reference", 
                                    "distributions/" = "Planting only", 
                                    "adapted/distributions/" = "Planting & maturity", 
                                    "cycle_adapted/distributions/" = "Optimal cycle") %>% 
           factor(labels = names(cal_sc)[-1]))


pB <- ggplot(ssp2, aes(x = YEAR, y = FF)) + theme_classic() + 
  geom_line(aes(colour = Adaptation), size = 0.75) + 
  geom_hline(yintercept = 1, linetype = 2) + 
  facet_grid(Stress ~ Scenario, scales = "free_y", 
             labeller = labeller(Scenario = c("ssp126" = "SSP1-2.6", 
                                              "ssp370" = "SSP3-7.0", 
                                              "ssp585" = "SSP5-8.5"), 
                                 Stress = as_labeller(grp_lbls, default = label_parsed))) + 
  scale_colour_manual(values = cal_sc) + 
  labs(x = "", y = "Exposure fold-change vs.\nreference calendar", colour = "", 
       tag = "A") + 
  guides(colour = "none") + 
  theme(axis.title = element_text(face = "bold"), 
        strip.text = element_text(face = "bold"))


gp <- arrangeGrob(ggplotGrob(pB), ggplotGrob(pA), 
                  layout_matrix = matrix(1:2, ncol = 1))
png("figures/Fig_exposure.png", width = 8, height = 7.5, 
    units = "in", res = 200)
plot(gp)
dev.off()



ref2 <- ssp %>% 
  filter(Adaptation == "reference_cal/distributions/") %>% 
  group_by(Scenario, Model, Stress, YEAR) %>%
  summarise(Time = mean(Time),
            .groups = "drop")
ssp3 <- ssp %>% 
  filter(Adaptation != "reference_cal/distributions/") %>% 
  group_by(Adaptation, Scenario, Model, Stress, YEAR) %>%
  summarise(Time = mean(Time),
            .groups = "drop") %>% 
  inner_join(ref2, by = c("YEAR", "Stress", "Scenario", "Model")) %>% 
  mutate(FF = Time.x/Time.y, 
         Stress = factor(Stress, levels = c("Optimal", "Moderate", "Severe"), ordered = TRUE), 
         Adaptation = recode_factor(Adaptation, "distributions/" = "Planting only", 
                                    "adapted/distributions/" = "Planting & maturity", 
                                    "cycle_adapted/distributions/" = "Optimal cycle") %>% 
           factor(labels = names(cal_sc)[-1]))

png("figures/FigS_exposure_ssp126.png", width = 9, height = 4, 
    units = "in", res = 200)
ggplot(filter(ssp3, Scenario == "ssp126"), aes(x = YEAR, y = FF)) + theme_classic() + 
  geom_line(aes(colour = Adaptation), size = 0.5) + 
  geom_hline(yintercept = 1, linetype = 2) + 
  facet_grid(Stress ~ Model, scales = "free_y", 
             labeller = labeller(Stress = as_labeller(grp_lbls, default = label_parsed), 
                                 Model = as_labeller(model_lbls, default = label_parsed))) + 
  scale_colour_manual(values = cal_sc[-1]) + 
  labs(x = "", y = "Exposure fold-change vs. reference calendar", colour = "", 
       title = "SSP1-2.6") + 
  theme(axis.title = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.text = element_text(face = "bold"))
dev.off()

png("figures/FigS_exposure_ssp370.png", width = 9, height = 4, 
    units = "in", res = 200)
ggplot(filter(ssp3, Scenario == "ssp370"), aes(x = YEAR, y = FF)) + theme_classic() + 
  geom_line(aes(colour = Adaptation), size = 0.5) + 
  geom_hline(yintercept = 1, linetype = 2) + 
  facet_grid(Stress ~ Model, scales = "free_y", 
             labeller = labeller(Stress = as_labeller(grp_lbls, default = label_parsed), 
                                 Model = as_labeller(model_lbls, default = label_parsed))) + 
  scale_colour_manual(values = cal_sc[-1]) + 
  labs(x = "", y = "Exposure fold-change vs. reference calendar", colour = "", 
       title = "SSP3-7.0") + 
  theme(axis.title = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.text = element_text(face = "bold"))
dev.off()

png("figures/FigS_exposure_ssp585.png", width = 9, height = 4, 
    units = "in", res = 200)
ggplot(filter(ssp3, Scenario == "ssp585"), aes(x = YEAR, y = FF)) + theme_classic() + 
  geom_line(aes(colour = Adaptation), size = 0.5) + 
  geom_hline(yintercept = 1, linetype = 2) + 
  facet_grid(Stress ~ Model, scales = "free_y", 
             labeller = labeller(Stress = as_labeller(grp_lbls, default = label_parsed), 
                                 Model = as_labeller(model_lbls, default = label_parsed))) + 
  scale_colour_manual(values = cal_sc[-1]) + 
  labs(x = "", y = "Exposure fold-change vs. reference calendar", colour = "", 
       title = "SSP5-8.5") + 
  theme(axis.title = element_text(face = "bold"), 
        axis.text.x = element_text(angle = 45, hjust = 1), 
        strip.text = element_text(face = "bold"))
dev.off()
