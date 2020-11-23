
################################################################################

# create variables for directories
set.up <-
  function(base_directory,    # directory containing UA .zip files
           year,              # year of interest
           change = F         # using UA change (UAC) or normal UA. Default: UA
  ) {
    
    # UA or UAC (UA_change)?
    if (
      change == F
    ) u <- "UA" else u <- "UAC"
    
    # Input directory containing UA .zip files 
    indir <- 
      paste0(base_directory, u, year, "/")
    
    # Temp directory used for unzipping the UA-files
    tmpdir <- 
      paste0(base_directory,
             "temp/")
    if (
      !dir.exists(tmpdir)
    ) dir.create(tmpdir)
    
    # Directory for storing the results
    outdir <-
      paste0(base_directory,
             "Results_",
             u, year, "/")
    if (
      !dir.exists(outdir)
    ) dir.create(outdir)
    
    # return list of directories
    return(list("indir" = indir, 
                "tmpdir_glob" = tmpdir, 
                "outdir" = outdir,
                "base_dir" = base_directory,
                "year" = year))
  }

################################################################################

# empty temp directory
clean.up <-
  function(directory){
    require(dplyr)
    directory %>% 
      unlink(recursive = T)
  }

################################################################################

# load the qpkg or shape file
load.shape <-
  function(zip_list,
           base_dir,
           i,
           lyr
  ){
    
    # create temp folder for unzipping
    tmpdir <- 
      paste0(base_dir,
             strsplit(zip_list[i], "/")[[1]][3] %>% 
               sub(".{8}$", "", .)  
      )
    if (
      !dir.exists(tmpdir)
    ) dir.create(tmpdir)
    
    # unzip input file
    unzip(zipfile = zip_list[i], 
          exdir = tmpdir)
    
    # get file directory and load
    if(
      # check year
      set_up$year == 2012
    ) try({
      
      # loader for 2012 qpkg files:
      suppressWarnings(
        tmpshp <-
          tmpdir %>% 
          list.files(., 
                     recursive = T, 
                     full.names = T,
                     pattern = ".gpkg$"
          ) %>% 
          readOGR(dsn = .,
                  layer = ogrListLayers(.) %>%
                    .[lyr]),
        
      )
    }, silent = T
    
    # loader for 2006 and change shape-files
    ) else if(
      lyr == 2
    ) tmpshp <-
      # for the boundary file
      tmpdir %>% 
      list.files(., 
                 recursive = T, 
                 full.names = T,
                 pattern = "oundary.*shp$"
      ) %>% 
      readOGR(
      ) else tmpshp <-
      # for the land-use file
      tmpdir %>% 
      list.files(., 
                 recursive = T, 
                 full.names = T,
                 pattern = "2006.*shp$"
      ) %>% 
      readOGR()
    
    
    message("Load clear!") 
    # clean up temp directory
    clean.up(tmpdir)
    
    # return
    return(tmpshp)
  }

################################################################################

# add files in the list of zip-files together
combine.shapes <-
  function(zip_list,        # list of .zip files
           base_dir,        # base directory 
           target_layer     # target layer (land-use, boundary, core)
  ){
    # determine layer:
    if (target_layer == "land-use") lyr <- 1
    if (target_layer == "boundary") lyr <- 2
    if (target_layer == "core") lyr <- 3
    
    # load first file in zip list     
    combined <-
      load.shape(zip_list =  zip_list,
                 base_dir = base_dir, 
                 i = 1,
                 lyr = lyr)
    
    # merge the rest of the files
    for (f in 2:length(zip_list)) {
      
      try({  
        # load file
        to_combine <-
          load.shape(zip_list =  zip_list,
                     base_dir = base_dir, 
                     i = f,
                     lyr = lyr)
        
        # bind file to other files  
        combined <- 
          raster::bind(combined, to_combine)
      })
    }
    message("Combine clear!") 
    return(combined)
  }

################################################################################

# check for even number
is.even <- function(x) x %% 2 == 0

################################################################################
cities_loaded <- cities

combine.shapes2 <- function(cities_loaded) {
  
  # create a variable holding number of elements in cities_loaded
  N <- 
    cities_loaded %>% 
    length()
  
  # if the number of elements is odd, add the last element to the first one
  if (
    is.even(N) == F
  ) c(
    cities_loaded[[1]] <-
      raster::bind(cities_loaded[[1]], 
                   cities_loaded[[N]]),
    N <- N - 1
  )
  
  # get the position of each first element in N
  i <- seq(from = 1,
           to = N, 
           by = 2)
  
  # add every second element in cities loaded to every first one
  cl <- makeCluster(ncore)
  registerDoParallel(cl)
  
  cities_loaded <- 
  foreach(x = i) %dopar% {
    require(dplyr)
    require(rgdal)
    require(parallel)
    require(doParallel)
    
    cities_loaded[[x]] <-
      raster::bind(cities_loaded[[x]],
                   cities_loaded[[x + 1]])
    
  }
  stopCluster(cl)
  
  # return bound-together elements
  return(cities_loaded)
}

while (length(cities_loaded) > 1) {
  
  cities_loaded <- 
    combine.shapes2(cities_loaded)
  
}

plot(cities_loaded[[1]])
