################################################################################
#
# This document contains the workflow for creating the base layers required for 
# the implementation of land-use-change scenarios in QuickScan using urban-
# atlas data of European cities. 
# 
# For a description of which files are required see input_for_r.txt
# This skript requires the functions.R to be stored in the data directory
# Created by: Benjamin Labohm, Berlin 2019
#
################################################################################

# load required packages
library(rgdal)
library(raster)
library(dplyr)

################################################################################
# Load input files
################################################################################

# city name
city <- "Lodz"
# data direction:
data_dir <- "C:/LandOeko/ENABLE/"
# input direction
input_dir <- paste0(data_dir,
                    city,
                    "/input/")
# output direction
output_dir <- paste0(data_dir, 
                     city,
                     "/output/")
if (!dir.exists(output_dir)) dir.create(output_dir)
# load functions (make sure this loads newest version, in case you make changes)
data_dir %>% 
  list.files(recursive = T,
             full.names = T) %>% 
  grep("functions",. ,  value = T) %>% 
  .[1] %>% 
  source()

# get files

## land-use raster (IF urban atlas data is already rasterized)
rstfile <- 
  paste0(data_dir,
         city) %>% 
  list.files(recursive = T,
             full.names = T,
             pattern = "*tif$") %>% 
  grep("LU12",. ,  value = T) %>% 
  .[1]

## ELSE load land use shape
if (file.exists(rstfile)) lu <- 
  rstfile %>% 
  raster() else lu <- 
  paste0(data_dir,
         city) %>%
  list.files(recursive = T,
             full.names = T,
             pattern = ".shp$") %>% 
  grep("LU12",. ,  value = T) %>% 
  readOGR(stringsAsFactors = F) %>% 
  rasterize.ua(output_dir, .)


## write land-use raster to output folder
writeRaster(lu,
            paste0(output_dir,
                   "LU_2012.tif"),
            datatype = "INT2U",
            format = "GTiff",
            overwrite = T,
            prj = T,
            options = c("TFW=YES"))

## center location
center <- 
  paste0(data_dir,
         city) %>%  
  list.files(recursive = T,
             full.names = T,
             pattern = ".shp") %>% 
  grep("center",. ,  value = T) %>% 
  readOGR()
projection(center) <- projection(lu)

## distance to transport raster (previously created with proximity raster 
## function in qgis)
d2_transport <-
  input_dir %>% 
  list.files(recursive = T,
             full.names = T,
             pattern = ".tif") %>% 
  grep("trans",. ,  value = T) %>% 
  raster()
projection(d2_transport) <- projection(lu)

## distance to residential raster (previously created with proximity raster 
## function in qgis)
d2_densres <- 
  input_dir %>% 
  list.files(recursive = T,
             full.names = T,
             pattern = ".tif") %>% 
  grep("densres",. ,  value = T) %>% 
  raster()
projection(d2_densres) <- projection(lu)


################################################################################
# Step 1: Probability of Densification
# Create distance raster and calculate tertiles (for 'close', 'mid', 'far')
################################################################################

## calculate distance raster
d2center <- distanceFromPoints(lu, center@coords[1, c(1:2)])
## mask distance raster to get only values for discontinuous urban fabric classes
### select classes
duf_classes <- c(11210, 11220, 11230, 11240)
### create mask
duf_mask <- create.mask(lu, duf_classes)
### mask distance raster
d2center_masked <- mask(d2center, 
                        duf_mask) 
## calulate tertiles
d2center_ter <- d2center_masked %>% calc.ter()
## write distance raster in tertiles to file
writeRaster(d2center_ter,
            paste0(output_dir, "d_res2center.tif"),
            datatype = "INT2S",
            format = "GTiff",
            overwrite = T)

################################################################################
# Step 1: Probability of Densification
# Group urban fabric classes in density classes:
# 1: Class 11100: very dense,  2: Classes 11210-11220: dense, 
# 3: Classes 11230-11240: low dense
################################################################################

# create reclassification table
rcl_d <- matrix(
  data = c(
    -Inf, 11098, 0,
    11099, 11101, 1,
    11209, 11221, 2,
    11229, 11241, 3,
    11242, Inf, 0),
  nrow = 5,
  ncol = 3,
  byrow = T)
# reclassify land-use raster
dens_rst <- reclassify(x = lu,
                       rcl = rcl_d)
# write density raster to file
writeRaster(dens_rst,
            paste0(output_dir, "dens_resid.tif"),
            datatype = "INT2S",
            format = "GTiff",
            overwrite = T)

################################################################################
# Step 1: Probability of Densification
# Create distance to transport raster and calculate tertiles 
# (for 'close', 'mid', 'far') for:
#       - discontinuous residential (11240, 11230, 11220 and 11210)
#       - arable land, pastures and forest (21000, 23000, 31000) 
################################################################################

# create transportation raster (classes 12210 and 12220) 
# for creation of distance to transport raster in qgis!
#transport_classes <- c(12210, 12220)
#transport_rst <- create.mask(lu, transport_classes)
##calculate distance to transport line (takes ~1.5h; not parallizable faster 
# in QGIS. But beware: QGIS takes map units(Pixels) not meters. Influence on 
# tertile calculation?)
#d2_transport <- distance(transport_rst)
#writeRaster(d2_transport,
#            paste0(data_dir, "distance_trans.tif"),
#            datatype = "INT2S",
#            format = "GTiff",
#            overwrite = T)

# mask distance raster to get only values for low and very low density
# discontinuous urban fabric classes 11240, 11230, 11220 and 11210
ld_res_classes <- c(11240, 11230, 11220, 11210)
ld_res_mask <- create.mask(lu, ld_res_classes) 
## mask distance raster 
dld_res2trans <- mask(d2_transport, 
                      ld_res_mask)

## calulate tertiles
dld_res2trans_ter <- calc.ter(dld_res2trans)

## write distance rasters to file
writeRaster(dld_res2trans_ter,
            paste0(output_dir, "d_lowdens2trans.tif"),
            datatype = "INT2S",
            format = "GTiff",
            overwrite = T)

################################################################################
# Step 2: Probability of new construction
################################################################################
# mask distance raster to get only values for arable land, pastures and forest 
apf_classes <- c(21000, 23000, 31000, 32000)
apf_mask <- create.mask(lu, apf_classes)
## mask distance raster 
dapf2trans <- mask(d2_transport, 
                   apf_mask)
## calulate tertiles
dapf2trans_ter <- calc.ter(dapf2trans)

writeRaster(dapf2trans_ter,
            paste0(output_dir, "d_green2trans.tif"),
            datatype = "INT2S",
            format = "GTiff",
            overwrite = T)

################################################################################
# Create distance of arable land, pastures and forest (21000, 23000, 31000) 
# to medium and dense urban farbric raster (class 11100, 11210, 11220) 
################################################################################

# Create distance raster
## use arable, patures, forest classes from previous section to mask distance to
## dense residential
dapf2res <- mask(d2_densres, 
                 apf_mask)
## calulate tertiles
dapf2res_ter <- calc.ter(dapf2res)
## write results to file
writeRaster(dapf2res_ter,
            paste0(output_dir, "d_green2res.tif"),
            datatype = "INT2S",
            format = "GTiff",
            overwrite = T)

################################################################################
# Step 3: Conversion within built-up
################################################################################

built_classes <- c(12100, 11300, 12220, 12230, 13100, 13300, 13400)
built_mask <- create.mask(lu, built_classes) 
## mask distance to residential raster 
d_built2res <- mask(d2_densres, 
                    built_mask)

### mask distance to center raster
d_built2center <- mask(d2center, 
                       built_mask) 

## calulate tertiles
d_built2res_ter <- calc.ter(d_built2res)

d_built2center_ter <- calc.ter(d_built2center)

## write distance rasters to file
writeRaster(d_built2res_ter,
            paste0(output_dir, "d_built2res.tif"),
            datatype = "INT2S",
            format = "GTiff",
            overwrite = T)

writeRaster(d_built2center_ter,
            paste0(output_dir, "d_built2center.tif"),
            datatype = "INT2S",
            format = "GTiff",
            overwrite = T)

################################################################################
# Create no change layers
################################################################################
# load lu change and protection files
 
if (paste0(input_dir, "no_change.tif") %>% 
    file.exists() == F & paste0(output_dir, "no_change.tif") %>% 
    file.exists() == F) rasterize.nc(paste0(data_dir,
                                            city), 
                                     output_dir)
