
# LOAD PACKAGES
library(tidyverse)
library(rgdal)

# empty R's temp directory
tempdir() %>%
  dirname() %>%
  list.files(full.names = T, recursive = T)# %>%
  file.remove()

################################################################################
# PREPARATION:
# for Linux: MAKE SURE USB DRIVE IS MOUNTED CORRECLTY!
#
# Directories must be named UA (urban atlas) or UAC (urban atlas change) +
# year!
#
# Change the following variables to change input directory / year/ UA / UAC
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

#cleanup function
clean_up <-
  function(){
    tempdir %>% 
      list.dirs(full.names = T, recursive = T) %>% 
      unlink(recursive = T)
  }

# list of .zip files
z_list <-
  indir %>% 
  list.files(full.names = T) 

################################################################################
# Start here (after executing above code once):
# 1.)
# read first boundary layer
# clean up
clean_up()
# unzip
unzip(zipfile = z_list[1], exdir = tempdir %>% sub("p/", "p", .))

# 2.)
# get file directory and load
bound <-  
  tempdir %>% 
  list.files(., 
             recursive = T, 
             full.names = T,
             pattern = ".gpkg$"
  ) %>% 
  readOGR(dsn = .,
          layer = ogrListLayers(.) %>%
            .[3])

#df <- bound@data
# 3.)
# clean up
clean_up()

# layer names

################################################################################
# city 1 + city 49 + any other city will give row.name length error
################################################################################
# 4.)
# merge the rest of the boundaries
for (f in z_list[#c(48, 49, 50) 
                 2:length(z_list)
                 
]){
  #empty temp directory
  clean_up()
  
  # unzip element from .zip file list to temp directory
  f %>% 
    unzip(zipfile = ., exdir = tempdir %>% sub("p/", "p", .))
  
 # geopackage file:
  qpkg <-
    tempdir %>%
    list.files(.,
               recursive = T,
               full.names = T,
               pattern = ".gpkg$")

  

  try({  
  # boundary layer in gpkg file
  lyr <-
    qpkg %>%
    ogrListLayers() %>%
    .[3]

  # load file
  b <-
    qpkg %>%
    readOGR(.,
            layer = lyr
    )
  message("load clear")

  # merge files together  
  #ifelse(
   # exists("bound"),
    bound <-
      raster::bind(bound, b)#,
    
    #bound <-
     # b
  #)
  message("union clear")

# df <-
#   df %>%
#   rbind(b@data)
# 
# bound@data <-
#   df
 
#  message("rbind clear")
  
  # write the boundary layer to file
  writeOGR(obj = bound,
           dsn = "E:/Results_UA2012/urban_core.shp",
           layer = "E:/Results_UA2012/urban_core",
           driver = "ESRI Shapefile",
           overwrite_layer = T)
  message("save clear")
      
  # clean up
  clean_up()
  })}

# write.csv(df, 
#           paste0(outdir,
#                  "UA2012_data.csv", 
#                  row.names = F))
#plot(bound)

################################################################################
# write to file after each step
# merge() ? -> überlappende polygone (union -> cutted)
# test for errer condition

# write the boundary layer to file
# writeOGR(obj = bound,
#          dsn = "E:/Results_UA2012/boundaries.shp",
#          layer = "E:/Results_UA2012/boundaries",
#          driver = "ESRI Shapefile",
#          overwrite_layer = T)
