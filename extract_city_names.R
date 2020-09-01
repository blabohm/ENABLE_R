
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

# extract clean city names, city code and .zip archive name
cn <-
  indir %>% 
  list.files() %>% 
  as_tibble() %>% 
  transmute(file_name = value) %>% 
  separate(file_name, "_", into = c("code", "city"), 
           extra = "warn", remove = F) %>% 
  select(city , code, file_name) %>% 
  mutate(city = gsub(".zip", "", city))

# check if everything went well
head(cn)

# write to .csv file
write.csv(cn, 
          file = paste0("C:/LandOeko/workflow/city_names_",
                        u, t, ".csv"), 
          row.names = F, quote = F)
