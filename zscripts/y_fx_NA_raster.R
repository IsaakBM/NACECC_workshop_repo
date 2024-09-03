# This code was written by Isaac Brito-Morales (ibrito@conservation.org)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!


rs_split <- function(rs, outdir) {
  
  library(dplyr)
  library(terra)
  library(stringr)
  
  dir_rs <- list.files(path = rs, 
                       pattern = "*.tif", 
                       all.files = TRUE, 
                       full.names = TRUE, 
                       recursive = FALSE)
  FF <- lapply(dir_rs, function(x){
    rs01 <- rast(x)
    rs02 <- terra::rotate(rs01)
    rs03 <- terra::crop(rs02, ext(-70, -10, 33, 67)) # North Atlantic
  # Generate output file name based on input file name
    # output_name <- gsub(pattern = "\\.tif$", replacement = "_processed.tif", basename(x))
    output_name <- paste0(outdir, basename(x))
    terra::writeRaster(rs03, output_name)
  })
}


rs_split(rs = "data_raw/fsle_rs00",
         outdir = "data_raw/fsle_rs/")