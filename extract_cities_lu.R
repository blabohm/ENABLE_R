
# LOAD PACKAGES
library(tidyverse)
library(rgdal)
library(parallel)
library(doParallel)

# empty R's temp directory
# tempdir() %>%
#   dirname() %>%
#   list.files(full.names = T, recursive = T) %>%
# file.remove()

################################################################################
# PREPARATION:
# for Linux: MAKE SURE (USB-)DRIVE IS MOUNTED CORRECLTY!
#
# Directories must be named UA (urban atlas) or UAC (urban atlas change) +
# year!
#
# Change the following variables to change input directory / year/ UA / UAC
# Input directory with "cities/"-directory containing UA .zip files
################################################################################

# load functions:
source("C:/Users/labohben/Documents/GitHub/ENABLE_R/extract_lu/functions.R")

# set up required directories
set_up <-
  set.up(
    base_directory = "E:/",
    year = 2012,
    change = F
  )

# list of .zip files
z_list <-
  set_up$indir %>% 
  list.files(full.names = T) 

# bind together layers in parallel
c <- detectCores() - 1 
cl <- makeCluster(c)
registerDoParallel(cl)

cities <-
  foreach(z_list[1:24]) %dopar% {
    require(dplyr)
    require(rgdal)
    require(parallel)
    require(doParallel)
    combine.shapes(zip_list = z_list,
                   base_dir = set_up$base_dir,
                   target_layer = "core") # target layer (land-use, boundary, core)
  
}

stopCluster(cl)



