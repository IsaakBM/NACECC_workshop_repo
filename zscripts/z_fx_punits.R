# This code was written by Isaac Brito-Morales (ibrito@conservation.org)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# Function to create square or heaxagonal planning units for your area of interest.

library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyr)
library(transformr)
library(stringr)
library(readr)

source("zscripts/z_helpFX.R")

########################
####### 
########################
# Create polygon on the area of interest
  bbox <- st_bbox(c(xmin = -54, xmax = -35, ymin = 35, ymax = 57), 
                  crs = st_crs(LatLon)) %>% # xmin = -54, xmax = -35, ymin = 40, ymax = 57
    st_as_sfc() %>% 
    st_transform(crs = robin)
  f_bbox <- bbox
# 
  CellArea <- 4 # in km2
  h_diameter <- 2 * sqrt((CellArea*1e6)/((3*sqrt(3)/2))) * sqrt(3)/2 # Diameter in m
  s_diameter <- sqrt(CellArea*1e6) # Diameter in m
  
# Create planning units for the whole region
  PUs <- st_make_grid(f_bbox,
                      square = F,
                      cellsize = c(h_diameter, h_diameter),
                      what = "polygons",
                      crs = st_crs(f_bbox)) %>%
    st_sf()
# Check cell size worked ok.
  print(paste0("Range of cellsize are ",
               round(as.numeric(range(units::set_units(st_area(PUs), "km^2")))[1])," km2 to ",
               round(as.numeric(range(units::set_units(st_area(PUs), "km^2")))[2])," km2"))
# 
  logi_PUs <- st_centroid(PUs) %>%
    st_intersects(naSF_rob) %>% 
    lengths > 0 # Get logical vector instead of sparse geometry binary
  PUs1 <- PUs[logi_PUs == FALSE, ]
  # st_write(obj = PUs1, dsn = "input_layers/boundaries/", layer = "PUs_NA_04km2", driver = "ESRI Shapefile")

########################
####### 
########################
# 
  g1 <- ggplot() +
    geom_sf(data = PUs, size = 0.05) +
    geom_sf(data = worldsf_rob, size = 0.05, fill = "grey20") +
    theme_bw() +
    coord_sf(xlim = c(st_bbox(worldsf_rob2)$xmin + 85000, st_bbox(worldsf_rob2)$xmax - 85000),
             ylim = c(st_bbox(worldsf_rob2)$ymin + 70000, st_bbox(worldsf_rob2)$ymax - 70000),
             expand = TRUE)