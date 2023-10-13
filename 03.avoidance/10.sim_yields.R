library(tidyverse)
library(abind)
library(splines)
library(splines2)
library(lubridate)
library(argparse)
library(parallel)

options(readr.show_progress = FALSE)


# Parse commandline arguments
parser <- ArgumentParser()
parser$add_argument("-s", "--scenario", type = "character")
args <- parser$parse_args()

# For testing
# args <- list("scenario" = "historical")
# args <- list("scenario" = "ssp585")

# For navigation
dir1 <- "data/ISIMIP3a/distributions"
dir2 <- paste0("data/ISIMIP3a/relative/", args$scenario)
if (!dir.exists(dir2)) dir.create(dir2)

models <- list.dirs(dir1, recursive = FALSE) %>% 
  str_split("/") %>% 
  sapply(tail, n = 1)


# Gather scaling factors --------------------------------------------------
reg_dat <- read_rds("data/historical/reg_dat_CUB.rds")
bn <- which(str_detect(names(reg_dat), "^bspl"))
varieties <- reg_dat$Variety %>% unique() %>% sort()
prcp_sc <- list("center" = attr(reg_dat$PRCP, "scaled:center"), 
                "scale" = attr(reg_dat$PRCP, "scaled:scale"))
temp_sc <- list("center" = sapply(bn, function(x) attr(reg_dat[[x]], "scaled:center")), 
                "scale" = sapply(bn, function(x) attr(reg_dat[[x]], "scaled:scale")))

HOURS_c <- read_rds("data/historical/regression_data.rds") %>% 
  `$`("Ly") %>%
  simplify2array() %>% 
  rowMeans()


# Prepare spline bases ----------------------------------------------------
H_prcp <- bs(seq(min(reg_dat$PRCP), max(reg_dat$PRCP), length.out = 100), 
             df = 5, intercept = FALSE)

TEMP_range <- c(-1, 41)
TEMP_eval <- -1:41
basis_par <- list("Order" = 3L, 
                  "Knots" = quantile(TEMP_eval, seq(0, 1, length.out = 5L)))

H <- bSpline(TEMP_eval, knots = basis_par[[2]][2:(length(basis_par[[2]]) - 1)], 
             degree = basis_par[[1]], intercept = FALSE)

bs_obj <- fda::basisfd(type = "bspline", rangeval = TEMP_range, nbasis = ncol(H), 
                       params = attr(H, "knots"), 
                       basisvalues = list(list(args = TEMP_eval, values = H)))
Q <- fda::inprod(bs_obj, bs_obj, 0, 0)

rm(reg_dat, bn, bs_obj); gc()


# Process weather data ----------------------------------------------------
# Process planting dates
pdates <- map_df(models, function(m) {
  paste0(dir1, "/", m, "/", args$scenario, "/growing_seasons.csv") %>% 
    read_csv(col_types = cols()) %>% 
    mutate(PDAY = paste(YEAR_P, MONTH_P, DAY_P, sep = "-") %>% 
             ymd() %>% yday(), 
           Model = m) %>% 
    dplyr::select(Model, County, YEAR_P, PDAY)
})

# Process precipitation
prcp <- map_df(models, function(m) {
  list.files(paste0(dir1, "/", m, "/", args$scenario), "*_prcp\\.csv", 
             full.names = TRUE) %>% 
    map_df(function(f) {
      prcp <- read_csv(f, col_types = cols()) %>% 
        mutate(PRCP_sc = (PRCP - prcp_sc$center)/prcp_sc$scale)
      
      prcp_sp <- predict(H_prcp, newx = prcp$PRCP_sc) %>% 
        as.matrix()
      colnames(prcp_sp) <- paste0("PRCP5.", 1:ncol(prcp_sp))
      
      bind_cols(prcp, as_tibble(prcp_sp)) %>% 
        mutate(Model = m) %>% 
        dplyr::select(Model, County, YEAR, starts_with("PRCP5"))
    })
})

# Process temperature
temp <- map_df(models, function(m) {
  list.files(paste0(dir1, "/", m, "/", args$scenario), "*_temp\\.csv", 
             full.names = TRUE) %>% 
    map_df(function(f) {
      temp <- read_csv(f, col_types = cols())
      
      TEMP <- temp$Degree %>% unique() %>% sort()
      county <- temp$County[1]
      years <- temp$YEAR %>% unique() %>% sort()
      
      temp <- temp %>% 
        dplyr::select(-County) %>% 
        pivot_wider(names_from = "Degree", values_from = "Time") %>% 
        dplyr::select(-YEAR) %>% 
        as.matrix()
      
      temp <- temp - matrix(HOURS_c, nrow = nrow(temp), 
                            ncol = ncol(temp), byrow = TRUE)
      X_coef <- t(tcrossprod(solve(crossprod(H)), H) %*% t(temp)) %*% t(Q)
      X_coef <- (X_coef - matrix(temp_sc$center, nrow = nrow(X_coef), 
                                 ncol = ncol(X_coef), byrow = TRUE))/
        matrix(temp_sc$scale, nrow = nrow(X_coef), 
               ncol = ncol(X_coef), byrow = TRUE)
      colnames(X_coef) <- paste0("TEMP", basis_par[[1]], ".", 1:ncol(X_coef))
      
      tibble(Model = m, 
             County = county, 
             YEAR = years) %>% 
        bind_cols(as_tibble(X_coef))
    })
})


# Prepare predictions sans hybrids ----------------------------------------
### Dimensions:
###  1 = model
###  2 = county
###  3 = year
###  4 = bootstrap
###  5 = hybrid

# Pre-processed coefficients
pdate_coef <- read_rds("data/historical/CUB_pday.rds") %>% 
  pull(Value)
prcp_coef <- "data/historical/CUB_feffects_raw.csv" %>% 
  read_csv(col_types = cols(), progress = FALSE) %>% 
  filter(str_detect(Coefficient, "PRCP")) %>% 
  pivot_wider(names_from = "Coefficient", values_from = "Value") %>% 
  dplyr::select(-Bootstrap) %>% 
  as.matrix()

# Planting dates
pdates2 <- pdates %>% 
  split(~ Model) %>% 
  map(function(df) {
      df <- pivot_wider(df, names_from = "YEAR_P", values_from = "PDAY")
      temp <- as.matrix(df[, -(1:2)], dimnames = list(df$County, names(df)[-(1:2)]))
      rownames(temp) <- df$County
      temp
    }) %>% 
  abind(along = 0)

pdates_pred <- sapply(pdate_coef, function(x) x*pdates2, simplify = FALSE) %>% 
  abind(along = 4)

# Precipitation
prcp2 <- prcp %>% 
  pivot_longer(starts_with("PRCP"), names_to = "Basis", values_to = "Value") %>%
  split(~ Model) %>% 
  map(function(df) {
    split(df, ~ Basis) %>% 
      map(function(df2) {
          df2 <- pivot_wider(df2, names_from = "YEAR", values_from = "Value")
          temp <- as.matrix(df2[, -(1:3)], dimnames = list(df2$County, names(df2)[-(1:3)]))
          rownames(temp) <- df2$County
          temp
        }) %>% 
      abind(along = 3)
    }) %>% 
  abind(along = 0)

prcp_pred <- apply(prcp2, 1, function(pp) {
      apply(pp, 1, tcrossprod, y = prcp_coef, simplify = FALSE) %>% 
        abind(along = 0)
    }, simplify = FALSE) %>% 
  abind(along = 0)

# Temperature
temp2 <- temp %>% 
  pivot_longer(starts_with("TEMP"), names_to = "Basis", values_to = "Value") %>%
  split(~ Model) %>% 
  map(function(df) {
    split(df, ~ Basis) %>% 
        map(function(df2) {
            df2 <- pivot_wider(df2, names_from = "YEAR", values_from = "Value")
            temp <- as.matrix(df2[, -(1:3)], dimnames = list(df2$County, names(df2)[-(1:3)]))
            rownames(temp) <- df2$County
            temp
          }) %>% 
        abind(along = 3)
    }) %>% 
  abind(along = 0)


# Preparation for adding hybrids ------------------------------------------
### Pre-computed hybrid counts over various subsets of interest
hyb_counts <- read_rds("data/ISIMIP3a/hybrid_counts.rds")

### Variables to pass to the cluster for computing hybrid summaries
# Hybrid-specific temperature response coefficients
cc_hyb <- "data/historical/CUB_reffects_raw.csv" %>% 
  read_csv(col_types = cols(), progress = FALSE) %>% 
  filter(grpvar == "Variety") %>% 
  dplyr::select(-grpvar) %>% 
  mutate(term = str_remove_all(term, "[\\(\\)]")) %>% 
  pivot_wider(names_from = "term", values_from = "condval") %>% 
  split(.$Bootstrap) %>% 
  map(function(df) {
      mm <- as.matrix(df[, -(1:2)])
      dimnames(mm) <- list(df$grp, names(df)[-(1:2)])
      
      ms <- setdiff(varieties, rownames(mm))
      mm <- rbind(mm, 
                  matrix(as.numeric(NA), nrow = length(ms), 
                         ncol = ncol(mm), dimnames = list(ms, colnames(mm))))
      mm <- mm[order(rownames(mm)), ]
      mm
    }) %>% 
  abind(along = 3)
cc_hyb <- cc_hyb[, -1, ]

cc_fix <- "data/historical/CUB_feffects_raw.csv" %>% 
  read_csv() %>% 
  filter(str_detect(Coefficient, "bspl")) %>% 
  pivot_wider(names_from = "Coefficient", values_from = "Value") %>% 
  dplyr::select(-Bootstrap) %>% 
  as.matrix() %>% 
  t()
for (i in 1:(dim(cc_hyb)[1])) {
  cc_hyb[i, , ] <- cc_hyb[i, , ] + cc_fix
}

# Planting date and precipitation effects are hybrid-independent, so we 
# add them together and save repeating a large number of additions
non_temp <- pdates_pred + prcp_pred

# Hybrid weights using observations across the entire dataset
wt_all <- hyb_counts$all$Wt

# Hybrid weights using observations within decade of earliest appearance
wt_cohort <- hyb_counts$cohort$Wt
yr <- hyb_counts$cohort$Year

# Hybrid weights using observations only on hybrids released within the first or 
# last decades of the dataset
wt_group <- hyb_counts$group$Wt
gr <- hyb_counts$group$Group
vr <- hyb_counts$group$Variety

### A small amount of cleanup)
rm(H, H_prcp, hyb_counts, pdates, prcp, prcp_coef, prcp_sc, Q, temp, temp_sc, 
   HOURS_c, pdate_coef, pdates_pred, pdates2, prcp_pred, prcp2, TEMP_eval, 
   TEMP_range, cc_fix)
gc()


# Hybrid relative yield predictions ---------------------------------------
### Set up the cluster
cl <- makeCluster(20L, type = "FORK", 
                  outfile = paste0("logs/relative_", args$scenario, 
                                   "_", args$basis, ".log"))

# The most expensive step is calculating all of the individual hybrid predictions.
# The function computes this once and then provides three summaries based on 
# different combinations of hybrids.
ref <- parLapplyLB(cl, 1:(dim(cc_hyb)[3]), function(i) {
  cat("Bootstrap", i, "\n")
  
  i_tag <- if_else(i < 10, paste0("000", i), 
                   if_else(i < 100, paste0("00", i), 
                           if_else(i < 1000, paste0("0", i), as.character(i))))
  
  yr_lvls <- unique(yr) %>% sort()
  gr_lvls <- unique(gr) %>% sort()
  idx <- which(dimnames(cc_hyb)[[1]] %in% vr)
  
  med <- apply(cc_hyb[, , i], 1, function(x) {
        xx <- matrix(x, ncol = 1)
        apply(temp2, 1, function(tt) {
              apply(tt, 2, `%*%`, y = xx)
            }, simplify = FALSE) %>% 
          abind(along = 0) %>% 
          `+`(non_temp[, , , i])
      }, simplify = FALSE) %>% 
    abind(along = 4)
  
  ### `med` dimensions:
  ###  1 = model
  ###  2 = county
  ###  3 = year
  ###  4 = hybrid
  
  global <- apply(med, 1:3, weighted.mean, w = wt_all, na.rm = TRUE)
  write_rds(global, paste0(dir2, "/", args$basis, "_global_bootstrap", i_tag, ".rds"))
  rm(global); gc()
  
  group <- apply(med[, , , idx], 1:3, function(x) {
      sapply(gr_lvls, function(g) {
          weighted.mean(x[gr == g], wt_group[gr == g], na.rm = TRUE)
        })
    })
  write_rds(group, paste0(dir2, "/", args$basis, "_group_bootstrap", i_tag, ".rds"))
  rm(group); gc()
  
  return(TRUE)
})

stopCluster(cl)
