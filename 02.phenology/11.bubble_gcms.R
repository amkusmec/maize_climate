library(tidyverse)


state_names <- c("illinois", "iowa", "nebraska", "kansas")
states <- map_data("state", region = state_names)

county_df <- map_data("county") %>%
  semi_join(states, by = "region")

prism_files <- list.files("data/historical/prism_subset", "*tmax*", full.names = TRUE)

dates <- str_extract(prism_files, "[0-9]{6}")
yrs <- str_extract(dates, "^[0-9]{4}") %>% as.integer()
mns <- str_extract(dates, "[0-9]{2}$") %>% as.integer()

prism_files <- prism_files[yrs >= 1934 & yrs <= 2014 & mns >= 4 & mns <= 9]
dates2 <- str_extract(prism_files, "[0-9]{6}")
mns2 <- str_extract(dates2, "[0-9]{2}$") %>% as.integer()
prism_files <- split(prism_files, mns2)

dat <- map_df(prism_files[[4]], function(f) {
      cat(f, "\n")
      read_csv(f, col_types = cols()) %>%
        select(Year:Month, Cell:Value)
    }) %>%
  filter(!is.na(Value)) %>%
  group_by(Month, Cell, Longitude, Latitude) %>%
  summarise(Pre = mean(Value[Year <= 1958]),
            Pre1940 = mean(Value[Year >= 1940 & Year <= 1958]),
            Post = mean(Value[Year > 1958]),
            .groups = "drop")


models <- list.dirs("data/ISIMIP3a/dailys", full.names = FALSE, recursive = FALSE)
d2 <- map_df(models, function(m) {
  list.files(paste0("data/ISIMIP3a/dailys/", m, "/historical"),
             "*", full.names = TRUE) %>%
    map_df(function(f) {
      cat(m, f, "\n")
      read_csv(f, col_types = cols()) %>%
        filter(YEAR >= 1934, YEAR <= 2014) %>%
        filter(MONTH >= 4, MONTH <= 9) %>%
        group_by(MONTH, YEAR) %>%
        summarise(TMAX = mean(TMAX),
                  .groups = "drop_last") %>%
        summarise(Pre = mean(TMAX[YEAR <= 1958]),
                  Pre1940 = mean(TMAX[YEAR >= 1940 & YEAR <= 1958]),
                  Post = mean(TMAX[YEAR > 1958]),
                  .groups = "drop") %>%
        mutate(Cell = str_extract(f, "[0-9]{6}"),
               Model = m)
    })
})

write_csv(d2, "data/bubble_gcms.csv")
d2 <- read_csv("data/bubble_gcms.csv", col_types = cols())

tests <- d2 %>% 
  group_by(Model, MONTH) %>% 
  group_modify(~ {
      test1 <- t.test(.x$Pre, .x$Pre1940, paired = TRUE)
      test2 <- t.test(.x$Post, .x$Pre, paired = TRUE)
      
      tibble(Test = rep(c("Pre", "Post"), each = 5), 
             Variable = rep(c("Estimate", "Stderr", "Pvalue", "Lower", "Upper"), times = 2), 
             Value = c(test1$estimate, test1$stderr, test1$p.value, 
                       test1$conf.int[1], test1$conf.int[2], 
                       test2$estimate, test2$stderr, test2$p.value, 
                       test2$conf.int[1], test2$conf.int[2]))
    }) %>% 
  ungroup()
attr(tests$Value, "names") <- NULL

tests <- tests %>% 
  pivot_wider(names_from = "Variable", values_from = "Value")



hist <- list.files("data/historical/prism_dailys", "*", full.names = TRUE) %>%
  map_df(function(f) {
    read_csv(f, col_types = cols()) %>%
      filter(MONTH >= 4, MONTH <= 9) %>%
      group_by(MONTH, YEAR) %>%
      summarise(TMAX = mean(TMAX),
                .groups = "drop_last") %>%
      summarise(Pre = mean(TMAX[YEAR <= 1958]),
                Pre1940 = mean(TMAX[YEAR >= 1940 & YEAR <= 1958]),
                Post = mean(TMAX[YEAR > 1958]),
                .groups = "drop") %>%
      mutate(Cell = str_extract(f, "[0-9]{6}"))
  })
write_csv(hist, "data/bubble_hist.csv")
hist <- read_csv("data/bubble_hist.csv", col_types = cols())


# Trial locations and dates
trials <- read_csv("data/historical/trials_noI.csv", col_types = cols())

# Get PRISM-county mappings
prism_files <- list.files("data/historical/prism_dailys", "*\\.csv") %>%
  str_split("_") %>%
  map_chr(`[`, 2) %>%
  as.integer()
prism_counties <- list.files("data/historical/prism_subset", "*\\.csv", full.names = TRUE) %>%
  `[`(1) %>% 
  read_csv(col_types = cols()) %>%
  filter(Cell %in% prism_files) %>%
  distinct(COUNTYNS, Cell) %>% 
  filter(COUNTYNS %in% trials$County)


hist_diff <- inner_join(hist, prism_counties, by = "Cell") %>% 
  group_by(COUNTYNS, MONTH) %>% 
  summarise(across(c(Pre, Post), ~ mean(.x, na.rm = TRUE)), 
            .groups = "drop") %>% 
  mutate(Diff = Post - Pre) %>% 
  group_by(MONTH) %>% 
  summarise(Lower = min(Diff), 
            Upper = max(Diff), 
            Mean = mean(Diff), 
            .groups = "drop") %>% 
  filter(MONTH >= 6, MONTH <= 8) %>% 
  mutate(Model = "Historical") %>% 
  select(Model, everything())

gcm_diff <- inner_join(d2, prism_counties, by = "Cell") %>% 
  group_by(Model, COUNTYNS, MONTH) %>% 
  summarise(across(c(Pre, Post), ~ mean(.x, na.rm = TRUE)), 
            .groups = "drop") %>% 
  mutate(Diff = Post - Pre) %>% 
  group_by(Model, MONTH) %>% 
  summarise(Lower = min(Diff), 
            Upper = max(Diff), 
            Mean = mean(Diff), 
            .groups = "drop") %>% 
  filter(MONTH >= 6, MONTH <= 8)

df <- bind_rows(hist_diff, gcm_diff) %>% 
  mutate(Model = factor(Model, levels = c("Historical", "gfdl-esm4", "ipsl-cm6a-lr", 
                                          "mpi-esm1-2-hr", "mri-esm2-0", 
                                          "ukesm1-0-ll"), ordered = TRUE))

ggplot(df) + theme_classic() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_linerange(aes(x = MONTH, ymin = Lower, ymax = Upper, colour = Model), 
                 size = 1, position = position_dodge(0.4)) + 
  geom_point(aes(x = MONTH, y = Mean, colour = Model), 
             size = 3, position = position_dodge(0.4)) + 
  scale_x_continuous(labels = c("June", "July", "August"), breaks = 6:8) + 
  scale_colour_brewer(type = "qual", palette = "Set2") + 
  labs(x = "", colour = "", 
       y = expression(bold("Mean temperature difference ("*degree*"C)")))
ggsave("figures/Fig_gcm_bubble.png", width = 5, height = 3, units = "in")
