library(tidyverse)
library(abind)
library(cropCalendars)
library(parallel)
library(argparse)

# Parse commandline arguments
parser <- ArgumentParser()
parser$add_argument("-s", "--scenario", type = "character")
parser$add_argument("-m", "--model", type = "character")
args <- parser$parse_args()

# For testing
# args <- list("scenario" = "historical", "model" = "gfdl-esm4")
# args <- list("scenario" = "ssp126", "model" = "gfdl-esm4")

# Number of years to use for calculating "average" climate
window <- 30

# Issues:
#  - Documentation claims that you pass a vector for climate data. The example 
#    creates a matrix of dimension nyears x 365/366. Internally, the function 
#    uses a 1-dimensional index, assuming a vector as stated in the documentation. 
#    If you pass a matrix, R uses column-major indexing on a matrix with a 1-dimensional
#    index.
#  - Internal comments say that a 20-year average is calculated. It actually calculates
#    an average over everything you provide.

if (args$scenario == "historical") {
  clim_dat <- paste0("data/ISIMIP3a/subset/", args$model, 
                     "_", args$scenario, "_subset.rds") %>% 
    read_rds()
  
  pr_dat <- clim_dat$pr
  temp_dat <- 0.5*(clim_dat$tasmax + clim_dat$tasmin)
  
  rm(clim_dat); gc()
} else {
  # We need the last `window` years of historical data to calculate the first
  # growing seasons
  clim_dat1 <- paste0("data/ISIMIP3a/subset/", args$model, 
                      "_historical_subset.rds") %>% 
    read_rds()
  clim_dat2 <- paste0("data/ISIMIP3a/subset/", args$model, 
                      "_", args$scenario, "_subset.rds") %>% 
    read_rds()
  
  pr_dat <- abind(clim_dat1$pr, clim_dat2$pr, along = 3)
  temp_dat <- 0.5*(abind(clim_dat1$tasmax, clim_dat2$tasmax, along = 3) + 
                     abind(clim_dat1$tasmin, clim_dat2$tasmin, along = 3))
  
  rm(clim_dat1, clim_dat2); gc()
}

# Keep only the necessary years to decrease memory footprint
years <- dimnames(temp_dat)[[3]] %>% str_extract("[0-9]{4}")
u_years <- unique(years)

if (args$scenario == "historical") {
  year1 <- 1934 - window
} else {
  year1 <- 2015 - window
}

idx <- which(as.integer(years) < year1)

pr_dat <- pr_dat[, , -idx]
temp_dat <- temp_dat[, , -idx]

# Create the new year index
gc()
years <- dimnames(temp_dat)[[3]] %>% str_extract("[0-9]{4}")
u_years <- unique(years)

# Calculate the crop season for each grid cell using a `window` years rolling 
# average as "climate" input
idx_tbl <- expand_grid(ln = 1:(dim(temp_dat)[1]), 
                       lt = 1:(dim(temp_dat)[2]))

cal <- mcMap(function(ln, lt) {
  map_df(as.integer(u_years[-(1:window)]), function(tt) {
    start <- tt - window
    end <- tt - 1
    y_idx <- which(years %in% as.character(start:end))
    
    eg_climate <- calcMonthlyClimate(lat = as.numeric(dimnames(temp_dat)[[2]][lt]), 
                                     temp = temp_dat[ln, lt, y_idx], 
                                     prec = pr_dat[ln, lt, y_idx], 
                                     syear = start, eyear = end, incl_feb29 = TRUE)
    calcCropCalendars(lon = as.numeric(dimnames(temp_dat)[[1]][ln]), 
                      lat = as.numeric(dimnames(temp_dat)[[2]][lt]), 
                      mclimate = eg_climate, crop = "Maize") %>% 
      mutate(year = tt, 
             model = args$model, 
             scenario = args$scenario) %>% 
      filter(irrigation == "Rainfed")
  })
}, ln = idx_tbl$ln, lt = idx_tbl$lt, mc.cores = 30L)

write_csv(bind_rows(cal), 
          paste0("data/ISIMIP3a/cycle_adapted/calendars/", 
                 args$model, "_", args$scenario, "_calendar.csv"))
