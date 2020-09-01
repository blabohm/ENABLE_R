
# LOAD PACKAGES
library(tidyverse)
library(rgdal)


# tempdir() %>%
#   dirname() %>%
#   list.files(full.names = T, recursive = T)# %>%
#   file.remove()


# MAKE SURE USB DRIVE IS MOUNTED CORRECLTY!
# Input directory with "cities/"-directory containing UA .zip files
base_dir <-
  "E:/"

# UA or UAC (UA_change)?
u <- 
  "UA"

# year?
t <- 
  "2012"
# Input directory containing UA .zip files 
indir <- 
  paste0(base_dir, u, t, "/")

# Temp directory used for unzipping the UA-files
tempdir <- 
  paste0(base_dir,
         "temp/")
if (!dir.exists(tempdir)) dir.create(tempdir)

# Directory for storing the results
outdir <-
  paste0(base_dir,
         "Results_",
         u, t, "/")
if (!dir.exists(outdir)) dir.create(outdir)


sample <-
  indir %>% 
  list.files(full.names = T) %>% 
  .[1:3]

# read first boundary layer
unzip(zipfile = sample[1], exdir = tempdir %>% sub("p/", "p", .))
bound <-  
  tempdir %>% 
  list.files(., 
             recursive = T, 
             full.names = T,
             pattern = ".gpkg$") %>% 
  readOGR(dsn = ., 
          layer = ogrListLayers(.) %>% 
            .[2]) 
tempdir %>% 
  list.files(full.names = T, recursive = T) %>% 
  file.remove()

# merge the rest of the boundaries
for (f in sample[2:length(sample)]) {

f %>% 
  unzip(zipfile = ., exdir = tempdir %>% sub("p/", "p", .))

bound <-  
  tempdir %>% 
    list.files(., 
               recursive = T, 
               full.names = T,
               pattern = ".gpkg$") %>% 
    readOGR(dsn = ., 
            layer = ogrListLayers(.) %>% 
              .[2]) %>% 
    raster::union(., bound) 
    
  
# clean up
tempdir %>% 
  list.files(full.names = T, recursive = T) %>% 
  file.remove()

return()
}

plot(bound)
