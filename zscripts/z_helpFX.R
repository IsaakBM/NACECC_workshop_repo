# This code was written by Isaac Brito-Morales (ibrito@conservation.org)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

library(sf)
library(terra)
library(stringr)
library(dplyr)
library(data.table)
library(future.apply)
library(parallel)
library(doParallel)
library(foreach)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyr)
library(transformr)
library(readr)

########################
####### Projections
########################
  #
    LatLon <- "EPSG:4326"
    robin <- "ESRI:54030"

########################
####### Region
########################
  # Global
    worldsf_rob <- ne_countries(scale = "medium", returnclass = "sf") %>%
      st_transform(crs = robin) %>% 
      st_make_valid()
    worldsf_rob2 <- ne_countries(scale = "medium", returnclass = "sf") %>%
      st_make_valid() %>% 
      st_crop(xmin = -70, xmax = -5, ymin = 33, ymax = 80) %>% 
      st_transform(crs = robin)
  # 
    naSF_rob <- ne_countries(scale = "medium", returnclass = "sf") %>%
      st_make_valid() %>% 
      st_transform(crs = LatLon) %>% 
      st_crop(xmin = -70, xmax = -30, ymin = 45, ymax = 60) %>% 
      st_transform(crs = robin)