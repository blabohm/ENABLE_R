
# LOAD PACKAGES
library(tidyverse)
library(rgdal)


tempdir() %>%
  dirname() %>%
  list.files(full.names = T, recursive = T)# %>%
#   file.remove()

# MAKE SURE USB DRIVE IS MOUNTED CORRECLTY!
# Input directory with UA .zip files
indir <- 
  "/run/media/cities"

# Temp directory used for unzipping the UA-files
tempdir <- 
  "/run/media/temp"
if (!dir.exists(tempdir)) dir.create(tempdir)

# Directory for storing the results
outdir <-
  "/run/media/UA2012"
if (!dir.exists(outdir)) dir.create(outdir)


sample <-
  indir %>% 
  list.files(full.names = T) %>% 
  .[1:3]


s1 <- 
  sample[1] 

unzip(s1, list = T)[[1]][1] %>% 
  unzip(zipfile = s1,
        files = ., 
        exdir = tempdir)

qpkg <-  
  tempdir %>% 
  list.files(., 
             recursive = T, 
             full.names = T) %>% 
  readOGR(dsn = ., 
          layer = ogrListLayers(.) %>% 
            .[1]) 
qpkg
summary(qpkg)

# gather boundaries and merge them 
# gather 
