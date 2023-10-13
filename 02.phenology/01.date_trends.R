library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)


mat <- read_csv("TableS1_USDA_progress.csv", col_types = cols(), 
                col_select = c(Year:`Week Ending`, State, `Data Item`, Value)) %>%
  mutate(Period = str_remove(Period, "WEEK #") %>% as.integer(), 
         `Data Item` = word(`Data Item`, start = -1) %>% 
           factor(levels = c("PLANTED", "EMERGED", "SILKING", "DENTED", "MATURE", "HARVESTED"), 
                  ordered = TRUE), 
         YDAY = yday(`Week Ending`), 
         Value = Value/100) %>% 
  filter(`Data Item` != "DENTED", `Data Item` != "EMERGED")

ns <- mat %>% 
  count(State, Year, `Data Item`) %>% 
  filter(n >= 5)

yrs <- ns %>% 
  count(State, `Data Item`)

mat2 <- mat %>% 
  select(Year, State:YDAY) %>% 
  semi_join(ns, by = c("State", "Year", "Data Item")) %>% 
  nest(data = c(Value, YDAY)) %>% 
  rowwise() %>% 
  mutate(Model = list(tryCatch({
      nls(Value ~ SSlogis(YDAY, Asym, xmid, scal), data = data)
    }, error = function(e) return(NULL)))) %>% 
  ungroup() %>% 
  filter(!map_lgl(Model, is.null)) %>% 
  mutate(ASYM = map_dbl(Model, function(m) coef(m)[1] %>% unname()), 
         XMID = map_dbl(Model, function(m) coef(m)[2] %>% unname()), 
         SCAL = map_dbl(Model, function(m) coef(m)[3] %>% unname()), 
         X10 = XMID - SCAL*log(ASYM/0.1 - 1), 
         X50 = XMID - SCAL*log(ASYM/0.5 - 1), 
         X75 = XMID - SCAL*log(ASYM/0.75 - 1))


trends <- mat2 %>% 
  select(Year:`Data Item`, X10:X75) %>% 
  nest(data = c(Year, X10:X75)) %>% 
  rowwise() %>% 
  mutate(Model10 = list(lm(X10 ~ 1 + Year, data = data)), 
         Model50 = list(lm(X50 ~ 1 + Year, data = data)), 
         Model75 = list(lm(X75 ~ 1 + Year, data = data)), 
         P10 = list(summary(Model10)$coefficients[2, 4]), 
         P50 = list(summary(Model50)$coefficients[2, 4]), 
         P75 = list(summary(Model75)$coefficients[2, 4])) %>% 
  ungroup() %>% 
  mutate(across(starts_with("P"), ~ unlist(.x) %>% round(digits = 3)))


heat_plot <- trends %>% 
  mutate(B10 = map_dbl(Model10, function(m) coef(m)[2] %>% unname()), 
         B50 = map_dbl(Model50, function(m) coef(m)[2] %>% unname()), 
         B75 = map_dbl(Model75, function(m) coef(m)[2] %>% unname())) %>% 
  select(State:`Data Item`, P10:B75) %>% 
  mutate(Significant10 = P10 <= 0.05, 
         Significant50 = P50 <= 0.05, 
         Significant75 = P75 <= 0.05) %>% 
  select(-starts_with("P")) %>% 
  pivot_longer(B10:Significant75, names_to = "Percentage", values_to = "Value") %>% 
  mutate(Variable = if_else(str_detect(Percentage, "B"), "Trend", "Sig"), 
         Percentage = str_extract(Percentage, "[0-9]{2}")) %>% 
  pivot_wider(names_from = "Variable", values_from = "Value") %>% 
  mutate(Sig = as.logical(Sig), 
         Sig = paste0(round(Trend, digits = 3), if_else(Sig, "*", "")), 
         State = factor(State, levels = rev(sort(unique(State))), ordered = TRUE))


pA <- filter(heat_plot, Percentage == "50") %>% 
  ggplot(aes(x = `Data Item`, y = State)) + theme_classic() + 
    geom_tile(aes(fill = Trend), colour = "black") + 
    geom_text(aes(label = Sig), size = 3) + 
    scale_y_discrete(labels = str_to_title) + 
    scale_x_discrete(labels = str_to_title) + 
    scale_fill_gradient2(low = "green", high = "red") + 
    labs(x = "", y = "", fill = "d/yr", tag = "A") + 
    theme(legend.title = element_text(face = "bold"))


### Without emergence
periods2 <- mat2 %>% 
  select(Year:`Data Item`, X50) %>% 
  nest(data = c(`Data Item`, X50)) %>% 
  filter(map_int(data, nrow) == 4) %>% 
  rowwise() %>% 
  mutate(Periods = list(tibble(Period = c("Vegetative", "Grainfill", "Drydown"), 
                               Length = diff(data$X50)))) %>% 
  ungroup() %>% 
  select(-data) %>% 
  unnest(Periods)

period_trends2 <- periods2 %>% 
  nest(data = c(Year, Length)) %>% 
  rowwise() %>% 
  mutate(Model = list(lm(Length ~ 1 + Year, data = data)), 
         Slope = coef(Model)[2] %>% unname(), 
         P = summary(Model)$coefficients[2, 4] %>% unname()) %>% 
  ungroup() %>% 
  mutate(Sig = paste0(round(Slope, digits = 3), if_else(P <= 0.05, "*", "")), 
         State = factor(State, levels = rev(sort(unique(State))), ordered = TRUE), 
         Period = factor(Period, levels = c("Vegetative", "Grainfill", "Drydown"), 
                         ordered = TRUE))

pB <- ggplot(period_trends2, aes(x = Period, y = State)) + theme_classic() + 
  geom_tile(aes(fill = Slope), colour = "black") + 
  geom_text(aes(label = Sig), size = 3) + 
  scale_fill_gradient2(low = "green", high = "red") + 
  scale_y_discrete(labels = str_to_title) + 
  scale_x_discrete(labels = str_to_title) + 
  labs(x = "", y = "", fill = "d/yr", tag = "B") + 
  theme(legend.title = element_text(face = "bold"))


gsl2 <- mat2 %>% 
  select(Year:`Data Item`, X50) %>% 
  filter(`Data Item` %in% c("PLANTED", "HARVESTED")) %>% 
  nest(data = c(`Data Item`, X50)) %>% 
  filter(map_int(data, nrow) == 2) %>% 
  unnest(data) %>% 
  group_by(Year, State) %>% 
  summarise(GSL = diff(X50), 
            .groups = "drop") %>% 
  nest(data = !State) %>% 
  rowwise() %>% 
  mutate(Model = list(lm(GSL ~ 1 + Year, data = data)), 
         Slope = coef(Model)[2] %>% unname(), 
         P = summary(Model)$coefficients[2, 4] %>% unname()) %>% 
  ungroup() %>% 
  mutate(Sig = paste0(round(Slope, digits = 3), if_else(P <= 0.05, "*", "")), 
         State = factor(State, levels = rev(sort(unique(State))), ordered = TRUE))

pC <- ggplot(gsl2, aes(x = factor(1), y = State)) + theme_classic() + 
  geom_tile(aes(fill = Slope), colour = "black") + 
  geom_text(aes(label = Sig), size = 3) + 
  scale_fill_gradient2(low = "green", high = "red") + 
  scale_x_discrete(labels = "Growing season length") + 
  scale_y_discrete(labels = str_to_title) + 
  labs(x = "", y = "", fill = "d/yr", tag = "C") + 
  theme(legend.title = element_text(face = "bold"))


gp <- arrangeGrob(ggplotGrob(pA), ggplotGrob(pB), ggplotGrob(pC), 
                  layout_matrix = matrix(c(1, 1, 2, 3), nrow = 2, byrow = TRUE))
ggsave("figures/FigS_season_trends.png", 
       gp, width = 7, height = 4.5, units = "in")
