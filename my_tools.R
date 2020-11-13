
# LOAD PACKAGES
library(tidyverse)
library(rgdal)

# empty R's temp directory
empty.temp <- function()
{
  require(dplyr)
  tempdir() %>%
    dirname() %>%
    list.files(full.names = T, recursive = T) %>%
  file.remove()
}

clean_up <-
  function(tempdir){
    tempdir %>% 
      list.dirs(full.names = T, recursive = T) %>% 
      unlink(recursive = T)
  }