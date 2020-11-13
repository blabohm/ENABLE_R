
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
                "tmpdir" = tmpdir, 
                "outdir" = outdir,
                "base_dir" = base_directory))
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
             strsplit(z_list[i], "/")[[1]][3] %>% 
               sub(".{8}$", "", .)  
      )
    if (
      !dir.exists(tmpdir)
    ) dir.create(tmpdir)
    
    # unzip input file
    unzip(zipfile = zip_list[i], 
          exdir = tmpdir)
    
    # get file directory and load
    tmpshp <-
      tmpdir %>% 
      list.files(., 
                 recursive = T, 
                 full.names = T,
                 pattern = ".gpkg$"
      ) %>% 
      readOGR(dsn = .,
              layer = ogrListLayers(.) %>%
                .[lyr])
    
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
    for (f in 2:length(zip_list)){
      to_combine <-
        load.shape(zip_list,
                   tmpdir,
                   f,
                   lyr)
      
      try({  
        # merge files together  
        combined <-
          raster::bind(combined, to_combine)
        
        # write the boundary layer to file
        # writeOGR(obj = bound,
        #          dsn = "E:/Results_UA2012/urban_core.shp",
        #          layer = "E:/Results_UA2012/urban_core",
        #          driver = "ESRI Shapefile",
        #          overwrite_layer = T)
        # message("save clear")
        
        # clean up
        clean_up(tmpdir)
      })
    }
    return(combined)
    }