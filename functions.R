################################################################################
# Functions required to create base layers for QuickScan
################################################################################


################################################################################
# Calculate tertiles of any given raster
# input_rst = a raster with continuous numeric values 
################################################################################

calc.ter <- function(input_rst){
  # load packages
  require(raster)
  require(dplyr)
  ## calulate tertiles 
  tertile <- quantile(input_rst, probs = c(1 / 3, 2 / 3))
  
  ## reclassify matrix for tertiles
  rcl_t <- matrix(
    data = c(
      -Inf, tertile[1], 1,
      tertile[1] + .00001, tertile[2], 2,
      tertile[2] + .00001, Inf, 3),
    nrow = 3,
    ncol = 3,
    byrow = T)
  ## reclassify distance raster
  output_rst <- reclassify(x = input_rst,
                           rcl = rcl_t)
  output_rst[is.na(output_rst)] <- 0
  return(output_rst)
}

################################################################################
# Create a mask (keep only desired classes as 1, rest NA)
# input_rst = raster with numeric classes 
# keep_classes = a vector of desired classes to keep
################################################################################

create.mask <- function(input_rst, keep_classes){
  # load packages
  require(raster)
  require(dplyr)
  # sort classes in ascending order
  class_sort <- sort(keep_classes)
  # create reclassification matrix
  ## create proxy matrix with right dimensions
  rcl <- matrix(
    nrow = (class_sort %>% length * 2) + 1,
    ncol = 3
  )
  ## populate matrix with values ranging around target classes
  for (x in class_sort) {
    
    rcl[(which(class_sort == x) * 2) - 1, 2] <- x - 2
    rcl[(which(class_sort == x) * 2) - 1, 3] <- NA
    
    rcl[(which(class_sort == x) * 2), 1] <- x - 1
    rcl[(which(class_sort == x) * 2), 2] <- x + 1
    rcl[(which(class_sort == x) * 2), 3] <- 1
    
    rcl[(which(class_sort == x) * 2) + 1, 1] <- x + 2
    
  }
  ## set starting and ending values of matrix to Infinity
  rcl[1,1] <- -Inf
  rcl[rcl[,1] %>% length, 2] <- Inf
  
  # reclassify input raster
  output_rst <- reclassify(x = input_rst,
                           rcl = rcl)
  
  # return mask
  return(output_rst)
}

################################################################################
# Create a land use raster from the urban atlas land use shape file
################################################################################

rasterize.ua <- function(output_dir, input_shp) {
  # load required packages
  require(rgdal)
  require(dplyr)
  require(raster)
  
  # Create raster file 
  ## select land-use 2012 code from urban atlas file
  lu_2012 <- input_shp 
  lu_2012@data <- 
    lu_2012 %>% 
    .@data %>% 
    select(CODE2012) 
  ## convert land-use code to numeric values
  lu_2012@data$CODE2012 <- as.numeric(lu_2012@data$CODE2012)
  ## create proxy raster for extent and resolution
  ext_rst <- raster() 
  extent(ext_rst) <- extent(lu_2012)
  res(ext_rst) <- 5
  projection(ext_rst) <- projection(lu_2012)
  ## Rasterize the urban atlas land use
  lu_2012.rst <- rasterize(x = lu_2012,
                           y = ext_rst,
                           field = lu_2012@data$CODE2012)
  ## write results to .tif file
  writeRaster(lu_2012.rst,
              paste0(output_dir, "LU_2012.tif"),
              datatype = "INT2U",
              format = "GTiff",
              overwrite = T,
              prj = T,
              options = c("TFW=YES"))
  
  #return result
  return(lu_2012.rst)
}
################################################################################

rasterize.nc <- function(input_dir, output_dir) {
  # load required packages
  require(rgdal)
  require(dplyr)
  require(raster)
  
  # load projection raster
  proj_rst <-
    input_dir %>% 
    list.files(recursive = T,
               full.names = T,
               pattern = ".tif$") %>% 
    grep("LU12",. ,  value = T) %>% 
    raster()
  # load change and protection files
  lu_change <- 
    input_dir %>% 
    list.files(recursive = T,
               full.names = T,
               pattern = ".shp$") %>% 
    grep("LUC",. ,  value = T) %>% 
    readOGR()
  projection(lu_change) <- projection(proj_rst)
  
  protection <- 
    input_dir %>% 
    list.files(recursive = T,
               full.names = T,
               pattern = ".shp$") %>% 
    grep("protect",. ,  value = T) %>% 
    readOGR()
  projection(protection) <- projection(proj_rst)
  # create dummy raster
  ext_rst <- raster() 
  extent(ext_rst) <- extent(proj_rst)
  res(ext_rst) <- res(proj_rst)
  projection(ext_rst) <- projection(proj_rst)
  
  #rasterize shapefiles (est. 5min per shapefile)
  change_rst <- rasterize(lu_change, 
                          ext_rst)
  prot_rst <- rasterize(protection, 
                        ext_rst)
  
  #remove NA
  change_rst[is.na(change_rst)] <- 0
  prot_rst[is.na(prot_rst)] <- 0
  
  # create binary map (0/1)
  no_change <- change_rst + prot_rst
  no_change[no_change <= 2] <- 0
  no_change[no_change > 2] <- 1
  
  # write no change raster to file
  writeRaster(no_change,
              paste0(output_dir, "no_change.tif"),
              datatype = "INT2S",
              format = "GTiff",
              overwrite = T)
}