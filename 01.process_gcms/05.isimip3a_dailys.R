library(tidyverse)
library(parallel)
library(argparse)
source("src/haversine.R")

# Parse commandline arguments
parser <- ArgumentParser()
parser$add_argument("-m", "--model", type = "character")
parser$add_argument("-s", "--scenario", type = "character")
args <- parser$parse_args()

# # For testing
# args <- list("model" = "ipsl-cm6a-lr", "scenario" = "historical")

# PRISM cell information
cell_ids <- list.files("data/historical/prism_dailys", "*\\.csv") %>%
  str_split("_") %>%
  map_chr(`[`, 2) %>%
  as.integer()
trials <- read_csv("data/historical/trials_noI.csv") %>%
  distinct(State, County, .keep_all = TRUE) %>%
  select(-Year)
cells <- read_csv("data/historical/prism_subset/PRISM_ppt_stable_4kmM2_193203_bil_subset.csv") %>%
  filter(COUNTYNS %in% trials$County) %>%
  distinct(Cell, .keep_all = TRUE) %>%
  select(Cell, COUNTYNS, Longitude, Latitude) %>%
  filter(Cell %in% cell_ids)

# Link cells and grid points
cl_var <- read_rds(paste0("data/ISIMIP3a/subset/", args$model, 
                          "_", args$scenario, "_subset.rds"))

lon <- as.numeric(dimnames(cl_var[[1]])[[1]])
lat <- as.numeric(dimnames(cl_var[[1]])[[2]])
had_grid <- expand.grid(lon = lon,
                        lat = lat) %>%
  as.matrix()
cell_to_grid <- cells %>% 
  mutate(Distance = map2(.x = Longitude, .y = Latitude, function(x, y) {
      haversine(x, had_grid[, 1], y, had_grid[, 2]) %>%
        enframe(name = "Index", value = "Distance") %>%
        arrange(Distance) %>%
        slice(1:4L)
    })) %>%
  unnest(Distance)

grid_var <- cell_to_grid %>%
  split(.$Cell)

# Prepare directory structure
dir1 <- paste0("data/ISIMIP3a/dailys/", args$model, "/")
if (!dir.exists(dir1)) dir.create(dir1)
dir1 <- paste0(dir1, args$scenario, "/")
if (!dir.exists(dir1)) dir.create(dir1)

cl <- makeCluster(20L, 
                  outfile = paste0("logs/", args$model, "_", 
                                   args$scenario, "_dailys.log"))
clusterEvalQ(cl, {
  library(tidyverse)
  library(lubridate)
})
clusterExport(cl, c("had_grid", "lon", "lat", "cl_var", "args", "dir1"))

res <- parLapplyLB(cl, grid_var, function(cell_df) {
  cat("Cell", cell_df$Cell[1], "\n")
  
  # Weights
  wts <- (1/cell_df$Distance^2)
  wts <- wts/sum(wts)
  
  # Get the indices for the variable array
  y_idx <- sapply(cell_df$Index, function(i) which(lon == had_grid[i, 1]))
  x_idx <- sapply(cell_df$Index, function(i) which(lat == had_grid[i, 2]))
  idx <- cbind(y_idx, x_idx)
  
  # Calculate the weighted average
  avg <- lapply(cl_var, function(cl) {
      apply(cl, 3, function(x) sum(x[idx]*wts)) %>%
        unname()
    })
  
  tibble(YEAR = year(dimnames(cl_var[[1]])[[3]]), 
         MONTH = month(dimnames(cl_var[[1]])[[3]]), 
         DAY = day(dimnames(cl_var[[1]])[[3]]), 
         TMAX = avg$tasmax,
         TMIN = avg$tasmin,
         PRCP = avg$pr) %>%
    write_csv(paste0(dir1, "PRISM_", as.integer(cell_df$Cell[1]), "_daily.csv"))
  return(TRUE)
}, chunk.size = 1000)

stopCluster(cl)
