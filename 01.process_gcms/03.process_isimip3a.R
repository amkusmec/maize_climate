library(tidyverse)
library(ncdf4)
library(ncdf4.helpers)
library(PCICt)
library(abind)
source("src/haversine.R")
library(argparse)

# Parse commandline arguments
parser <- ArgumentParser()
parser$add_argument("-m", "--model", type = "character")
parser$add_argument("-s", "--scenario", type = "character")
args <- parser$parse_args()


cell_ids <- list.files("data/historical/prism_dailys", "*\\.csv") %>%
  str_extract("[0-9]{5,6}") %>%
  as.integer()
trials <- read_csv("data/historical/trials_noI.csv") %>%
  distinct(State, County, .keep_all = TRUE) %>%
  select(-Year)
cells <- read_csv("data/historical/prism_subset/PRISM_ppt_stable_4kmM2_193203_bil_subset.csv") %>%
  filter(COUNTYNS %in% trials$County) %>%
  distinct(Cell, .keep_all = TRUE) %>%
  select(Cell, COUNTYNS, Longitude, Latitude) %>%
  filter(Cell %in% cell_ids)

files <- list.files(paste0("data/ISIMIP3a/GCM/", 
                           args$model, "/", args$scenario), 
                    "*\\.nc", full.names = TRUE)
cvars <- sapply(files, function(f) {
      temp <- str_extract(f, c("pr", "tasmax", "tasmin"))
      temp[!is.na(temp)]
    }) %>% 
  unname()
files <- split(files, cvars)

cl_var <- lapply(files, function(f_list) {
    lapply(f_list, function(f) {
        cat(f, "\n", 
            file = paste0("logs/", args$model, "_", args$scenario, "_process.log"), 
            append = TRUE)
        
        # Get the climate variable
        cvar <- str_extract(f, c("pr", "tasmax", "tasmin"))
        cvar <- cvar[!is.na(cvar)]
        
        # Open the NetCDF database
        climate_output <- nc_open(f)
        
        # Collect the axis variables
        lat <- ncvar_get(climate_output, varid = "lat")
        lon <- ncvar_get(climate_output, varid = "lon")
        cl_time <- nc.get.time.series(climate_output, v = cvar, time.dim.name = "time")
        
        # Define our regions of interest
        lat_idx <- seq(which.min(abs(lat - min(cells$Latitude) + 2)), 
                       which.min(abs(lat - max(cells$Latitude) - 2)))
        lon_idx <- seq(which.min(abs(lon - min(cells$Longitude) + 2)), 
                       which.min(abs(lon - max(cells$Longitude) - 2)))
        
        # Retrieve only the values for locations/times of interest
        cl_var <- nc.get.var.subset.by.axes(climate_output, cvar, 
                                            axis.indices = list(X = lon_idx, 
                                                                Y = lat_idx))
        
        # Convert units
        #  - For temperature: K -> C
        #  - For precipitation: kg/m^2/s -> mm/day
        if (cvar == "tasmax" | cvar == "tasmin") cl_var <- cl_var - 273.15
        if (cvar == "pr") cl_var <- 86400*cl_var
        
        # Rename the dimensions
        dimnames(cl_var) <- list(lon[lon_idx], 
                                 lat[lat_idx], 
                                 as.character(cl_time))
        
        # Close the database connection
        nc_close(climate_output)
        
        return(cl_var)
      }) %>% 
    abind(along = 3)
})

write_rds(cl_var, paste0("data/ISIMIP3a/subset/", 
                         args$model, "_", args$scenario, "_subset.rds"))
