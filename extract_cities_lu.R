
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

t_layer <- "boundary" # target layer (land-use, boundary, ONLY 2012: core)

# set up required directories
set_up <-
  set.up(
    base_directory = "E:/",  # directory storing folder with zip files
    year = 2006,             # year (2006, 2012, 2006_2012)
    change = F               # looking at change?
  )

# list all .zip files
z_list <-
  set_up$indir %>% 
  list.files(full.names = T) 

# determine number of cores and subtract 
ncore <- detectCores() - 1

# create equally sized lists of zip files for each core
list_part <- 
  BBmisc::chunk(x = z_list[1:24], 
                n.chunks = ncore)

# unzip, load and bind together all layers in directory in parallel
cl <- makeCluster(ncore)
registerDoParallel(cl)

cities <-
  foreach(n = 1:ncore) %dopar% {
    require(dplyr)
    require(rgdal)
    require(parallel)
    require(doParallel)
    combine.shapes(zip_list = list_part[[n]],
                   base_dir = set_up$tmpdir_glob,
                   target_layer = t_layer) 
  }
stopCluster(cl)

# combine layers in cities list to one 
while (length(cities) > 1) {
  
  cities <- 
    combine.shapes2(cities)
  
}

# generate the path to save result
s_path <- paste0(set_up$outdir,
                 t_layer)

# write the created layer to file
writeOGR(obj = cities[[1]],
         dsn = paste0(s_path, ".shp"),
         layer = s_path,
         driver = "ESRI Shapefile",
         overwrite_layer = T)

