# This code was written by Isaac Brito-Morales (ibrito@conservation.org)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM =
# path = directory of netCDF files
# outdir = where to allocate files by model
# v = variable # nolint
# region = interest region for crop

  par_ncdf_2D_rs <- function(path, outdir, v, region, classic) { # nolint

    library(doParallel)
    library(foreach)
    library(ncdf4)
    library(ncdf4.helpers)
    library(PCICt)
    library(rgdal)
    library(terra)
    library(dplyr)
    library(magrittr)
    library(stringr)

    # Function that will flip a raster
    # (was thinking for models at 0.5deg like MPI-ESM1-2-HR)
    flip_rs <- function(data) {
        for (i in 1:nlyr(data)) {
              if (i == 1) {
                single <- subset(data, i)
                rs <- cbind(
                      terra::crds(single,
                                  df = FALSE,
                                  na.rm = TRUE) %>% as_tibble(),
                      terra::values(single,
                                  na.rm = TRUE,
                                  dataframe = TRUE) %>% as_tibble())
                rs[, 1] <- ifelse(rs[, 1] < 0, rs[, 1] + 180, rs[, 1] - 180)
                rs2 <- terra::rast(rs, type = "xyz")
                st <- rs2
              } else {
                single <- subset(data, i)
                rs <- cbind(
                      terra::crds(single,
                                  df = FALSE,
                                  na.rm = TRUE) %>% as_tibble(),
                      terra::values(single,
                                    na.rm = TRUE,
                                    dataframe = TRUE) %>% as_tibble())
                rs[, 1] <- ifelse(rs[, 1] < 0, rs[, 1] + 180, rs[, 1] - 180)
                rs2 <- terra::rast(rs, type = "xyz")
                st <- c(st, rs2)
              }
            }
          return(st)
        }

    ncdf_2D_rs <- function(nc, v = v, x = "lon", y = "lat") { # nolint
      # Extract data from the netCDF file
       nc <- ncdf4::nc_open(nc)
        dat <- ncdf4::ncvar_get(nc, v) # x, y, year
        dat[] <- dat
        rlon <- ncdf4::ncvar_get(nc, varid = x) %>% 
          range()
        rlat <- ncdf4::ncvar_get(nc, varid = y) %>% 
          range()
        X <- dim(dat)[1] # nolint
        Y <- dim(dat)[2] # nolint
        tt <- ncdf4.helpers::nc.get.time.series(nc,
                                                v = "time",
                                                time.dim.name = "time")
          tt <- as.POSIXct(tt)
          tt <- as.Date(tt)
        ncdf4::nc_close(nc)
      # Make a raster with the right dims to fill with lat&lon
        rs <- terra::rast(nrow = Y, ncol = X, extent = terra::ext(c(rlon, rlat)))
      # Fix orientation of original data
      # [and then create a raster with this fix orientation]
        drs <- terra::xyFromCell(rs, 1:terra::ncell(rs)) %>%
          as_tibble()
      # Create raster stacks of depths for every month
        rs_list <- list() # to allocate results # nolint
        st <- terra::rast()
        for (i in 1:length(tt)) { # nolint
          dt1 <- dplyr::bind_cols(drs,
                                   as.vector(dat[, , i])) %>%
                 magrittr::set_colnames(c("x", "y", v))
          dt1 <- terra::rast(dt1, type = "xyz")
          names(dt1) <- tt[i]
          st <- c(st, terra::flip(dt1))
          print(paste0(tt[i], " of ", length(tt)))
      }
    #
     if(classic == TRUE) {
       # st <- flip_rs(st) # this is for CMIP6 datasets
       # st <- terra::rotate(st) this is for AVISO
     } else {
       st <- st
     }
    #
     if (region == "SO") {
      st <- terra::crop(st, terra::ext(c(-180, 180, -90, -45)))
      terra::crs(st) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
      st <- terra::project(st,
                       "+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", # nolint
                       method = "near",
                       over = FALSE)
     } else if (region == "NAB") {
       st <- terra::crop(st, terra::ext(c(-65, 0, 20, 67)))
       terra::crs(st) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" # nolint
     } else if (region == "Mzb") {
       # st <- terra::crop(st, terra::ext(c(31, 60, -30, -7)))
       # st <- terra::crop(st, terra::ext(c(30, 60, -35, -7)))
       st <- terra::crop(st, terra::ext(c(30, 75, -35, -7)))
       terra::crs(st) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" # nolint
     } else {
      st <- st
     }
     return(st)
  }

    #
      files.nc <- list.files(path = path, pattern = ".nc", full.names = TRUE) # nolint
      files_list <- list() # to allocate results # nolint
    # Do a parallel analysis for every climatic file
      UseCores <- 5 # nolint
      cl <- parallel::makeCluster(UseCores)
      doParallel::registerDoParallel(cl)
      rs_list <- foreach(i = 1:length(files.nc), # nolint
                             .packages = c("terra",
                                           "ncdf4",
                                           "ncdf4.helpers",
                                           "PCICt", 
                                           "dplyr", 
                                           "magrittr", 
                                           "stringr")) %dopar% {
                          # Transform a netCDF file into a raster
                            single <- ncdf_2D_rs(files.nc[i], v = v) # nolint
                          # Defining  files' name
                            ns <- basename(files.nc[i]) # nolint
                            rs_file <- stringr::str_remove_all(string = ns,
                                                               pattern = ".nc")
                          # Writing Raster
                            terra::writeRaster(single,
                                               paste0(outdir, rs_file, ".tif"),
                                               overwrite = TRUE,
                                               filetype = "GTiff")
                   }
        parallel::stopCluster(cl)
        return(rs_list)
  }
  system.time(par_ncdf_2D_rs(path = "/home/sandbox-sparc/AVISO_monthly/2022",
                             outdir = "/scratch/sparc/",
                             v = "fsle_max",
                             region = "global", 
                             classic = FALSE))
  
  # system.time(par_ncdf_2D_rs(path = "data_raw/fsle_nc",
  #                            outdir = "/scratch/sparc/",
  #                            v = "fsle",
  #                            region = "Mzb", 
  #                            classic = FALSE))
