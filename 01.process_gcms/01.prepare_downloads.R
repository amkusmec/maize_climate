library(tidyverse)


# Parse file names --------------------------------------------------------
files <- read_lines("data/ISIMIP3a/file_list.txt") %>% 
  enframe(name = "Order", value = "File") %>% 
  mutate(FileName = str_split(File, "/") %>% 
           map_chr(`[`, 13)) %>% 
  separate("FileName", c("GCM", "Ensemble", "Adjustment", "Scenario", 
                         "Variable", "Geography", "Time", "Start", "End"), 
           sep = "_") %>% 
  mutate(Start = as.integer(Start), 
         End = str_remove(End, "\\.nc") %>% as.integer()) %>% 
  filter(Start >= 1880)


# Write download script ---------------------------------------------------
files <- files %>% 
  split(.$GCM) %>% 
  map(function(l) l %>% split(.$Scenario))

output_file <- "01.process_gcms/02.isimip3a_downloads.sh"
cat("#!/bin/bash", 
    "", 
    "cd data/ISIMIP3a", 
    "mkdir GCM", 
    "cd GCM", 
    "", 
    file = output_file, sep = "\n")

for (mm in files) {
  cat("mkdir", mm[[1]][["GCM"]][1], "\n", 
      file = output_file, sep = " ", append = TRUE)
  cat("cd", mm[[1]][["GCM"]][1], "\n", 
      file = output_file, sep = " ", append = TRUE)
  
  for (df in mm) {
    cat("mkdir", df$Scenario[1],  "\n", 
        file = output_file, sep = " ", append = TRUE)
    cat("cd", df$Scenario[1],  "\n", 
        file = output_file, sep = " ", append = TRUE)
    
    cat(paste("wget -c", df$File), 
        file = output_file, sep = "\n", append = TRUE)
    
    cat("cd ..\n\n", file = output_file, sep = " ", append = TRUE)
  }
  
  cat("cd ..\n\n", file = output_file, append = TRUE)
}
