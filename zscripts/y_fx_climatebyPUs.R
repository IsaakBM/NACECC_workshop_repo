# This code was written by Isaac Brito-Morales (ibrito@conservation.org)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

climatevar_feature <- function(rs_path, shp_path, outdir, proj.geo, ...) {

  library(terra)
  library(sf)
  library(exactextractr)
  library(dplyr)
  library(foreach)
  library(doParallel) 
  library(nngeo)
  library(stringr)
  
  # Function to replace NAs with nearest neighbor. Function written by Jason Everett
    fCheckNAs <- function(df, vari) {
      if (sum(is.na(pull(df, !!sym(vari))))>0){ # Check if there are NAs
        
        gp <- df %>%
          mutate(isna = is.finite(!!sym(vari))) %>%
          group_by(isna) %>%
          group_split()
        
        out_na <- gp[[1]] # DF with NAs
        out_finite <- gp[[2]] # DF without NAs
        
        d <- st_nn(out_na, out_finite) %>% # Get nearest neighbour
          unlist()
        
        out_na <- out_na %>%
          mutate(!!sym(vari) := pull(out_finite, !!sym(vari))[d])
        
        df <- rbind(out_finite, out_na)
        
      }
      return(df)
    }
  
  # Folder's structure
    rs_fsel <- list.files(path = rs_path, pattern = ".tif", full.names = TRUE)
  # 
    for(j in seq_along(rs_fsel)) {
      # Reading input file
        shp_file <- st_read(shp_path) %>% 
          st_transform(crs = terra::crs(proj.geo))
      # Read raster object
        rs_file <- rast(rs_fsel[j])
        crs(rs_file) <- terra::crs("EPSG:4326")
        weight_rs <- terra::cellSize(rs_file)
        rs_file <- terra::project(rs_file, y = terra::crs(proj.geo), method = "near")
        weight_rs <- terra::project(weight_rs, y = terra::crs(proj.geo), method = "near")
      # 
        if(sum(duplicated((names(rs_file)))) != 0) {
          names(rs_file) <- seq(from = as.Date(names(rs_file)[1]), 
                                to = as.Date(paste0(paste0(unlist(stringr::str_split(names(rs_file)[1], "-"))[1:2], collapse = "-"), "-",length(names(rs_file)))), 
                                by = "day")
          } else {
            rs_file
          }
        
      # Getting cost value by planning unit
        rs_bypu <- exact_extract(rs_file, 
                                 shp_file, 
                                 "weighted_mean", 
                                 weights = weight_rs, 
                                 append_cols = TRUE, 
                                 full_colnames = TRUE)
        rs_shp <- dplyr::right_join(shp_file, rs_bypu, "FID")
        colnames(rs_shp) <- c(stringr::str_remove_all(string = names(rs_shp), pattern = "weighted_mean."))
      # Begin the parallel structure      
        cores  <-  20
        cl <- makeCluster(cores)
        registerDoParallel(cl)  
        nms <- names(rs_shp)
        nms <- nms[nms != "geometry" & nms != "FID"]
        ls_df <- vector("list", length = length(nms))
        df1 <- foreach(i = 1:length(nms), .packages = c("terra", "dplyr", "sf", "exactextractr", "nngeo", "stringr")) %dopar% {
          single <- rs_shp %>% 
            dplyr::select(FID, nms[i])
          rs_sfInt <- fCheckNAs(df = single, vari = names(single)[2]) %>% 
            as_tibble() %>% 
            dplyr::arrange(FID) %>%
            dplyr::select(-FID, -geometry) # -isna column removed
          ls_df[[i]] <- rs_sfInt
        }
        stopCluster(cl)
        rs_sfInt <- do.call(cbind, df1) %>%
          as_tibble()
      # Files' name
        ns <- stringr::str_remove_all(basename(rs_fsel[j]), pattern = ".tif")
      # Write the outputs
        saveRDS(rs_sfInt, paste(outdir, ns, ".rds", sep = ""))
    }
}


# climatevar_feature(rs_path = "data_raw/fsle_rs",
#                    shp_path = "input_layers/boundaries/PUs_NA_04km2.shp",
#                    outdir = "data_rout/",
#                    proj.geo = "ESRI:54030")

climatevar_feature(rs_path = "/home/sandbox-sparc/NACECC-Initiative/data_raw/fsle_rs",
                   shp_path = "/home/sandbox-sparc/NACECC-Initiative/input_layers/boundaries/PUs_NA_04km2.shp",
                   outdir = "/scratch/sparc/",
                   proj.geo = "ESRI:54030")