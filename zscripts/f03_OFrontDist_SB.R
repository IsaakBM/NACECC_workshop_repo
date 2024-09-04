# This code was written by Isaac Brito-Morales (ibrito@conservation.org)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# source("zscripts/z_helpFX.R")
source("/home/sandbox-sparc/NACECC-Initiative/zscripts/z_helpFX.R")

neardist_sim <- function(pus, fsle_sf, fdata, cutoff, output) {
  
  library(sf)
  library(terra)
  library(stringr)
  library(dplyr)
  library(data.table)
  library(future.apply)
  library(parallel)
  library(doParallel)
  library(foreach)
  
  dist_fx <- function(PUs, fauna, ofdates, cutoff) {
    
    UseCores <- 5 # TO 5 IN LOCAL
    cl <- parallel::makeCluster(UseCores)
    doParallel::registerDoParallel(cl)
    lsout <- vector("list", length = nrow(fauna))
    ls_out <- foreach(x = 1:nrow(fauna),
                      .packages = c("terra",
                                    "dplyr", 
                                    "sf", 
                                    "stringr")) %dopar% {
                                      dist02 <- st_distance(fauna[x, ], ofdates, by_element = FALSE) %>%
                                        t() %>%
                                        as_tibble()
                                      # Get the upper front quantile of front
                                      qfront <- ofdates %>%
                                        as_tibble() %>% 
                                        dplyr::select(2) %>% 
                                        quantile(probs = cutoff, na.rm = TRUE) %>% 
                                        as.vector()
                                      
                                      final <- cbind(PUs[,1], ofdates[,2], dist02) %>% 
                                        as_tibble() %>% 
                                        dplyr::select(-geometry, -geometry.1) %>% 
                                        dplyr::arrange(.[[3]]) %>% 
                                        dplyr::filter(.[[2]] > qfront) %>% 
                                        dplyr::slice(1)
                                      
                                      lsout[[x]] <- final %>% 
                                        dplyr::rename_with(.cols = 1, ~"pxID") %>% 
                                        dplyr::rename_with(.cols = 2, ~"FrontMag") %>% 
                                        dplyr::rename_with(.cols = 3, ~"DistHFront") %>% 
                                        dplyr::mutate(group = as.character(unique(fauna$birdID)), 
                                                      dates = unique(fauna$date))
                                      
                                      
                                      
                                    }
    stopCluster(cl)
    FFF <- do.call(rbind, ls_out)
    return(FFF)
  }
  
  # Reading inputs
    PUs <- st_read(pus)
    sf1 <- readRDS(fsle_sf) %>% 
      dplyr::mutate(across(everything(), ~ .x * -1))
    nms <- names(sf1) %>% 
      stringr::str_extract(pattern = ".*(?=\\.)")
    colnames(sf1) <- nms
  # 
    vecFls <- list.files(path = fdata, pattern = ".rds", all.files = TRUE, full.names = TRUE, recursive = FALSE)
    OFdates <- stringr::str_remove(unlist(stringr::str_split(basename(fsle_sf), pattern = "_"))[6], pattern = ".rds")
    vecFls <- vecFls[stringr::str_detect(string = vecFls, pattern = OFdates) == TRUE]
    
    FFdf <-  future.apply::future_lapply(vecFls, future.scheduling = 5, FUN = function(x) { # TO 5 IN LOCAL
      # 
        LatLon <- "EPSG:4326"
        robin <- "ESRI:54030"
      # 
        fdata01 <- readRDS(x)
        if(is.data.frame(fdata01)) {
          fdata01
          } else {
            fdata01 <- fdata01[[1]]
          }
      # ocean fronts per day of each biodiversity data
      df01 <- sf1 %>% 
        dplyr::select(as.character(unique(fdata01$date)))
      # we need to create a vector with the unique date information
      Fdates <- unique(fdata01$date)
      FF <- vector("list", length = length(Fdates))
      for(j in seq_along(Fdates)) {
        # Filter the megafauna data for each date
        mmF <- fdata01 %>% 
          sf::st_as_sf(coords = c("x", "y"), crs = LatLon) %>% # from dataframe to sf object
          dplyr::filter(date == Fdates[j]) %>% # add the j here
          sf::st_transform(crs = robin)
        # Filter Front data for each date of the megafauna data
        OFCdates <- df01 %>% 
          dplyr::select(as.character(Fdates[j]))
        OFCdates <- cbind(PUs, OFCdates) %>% 
          st_transform(crs = robin)
        # 
        FF[[j]] <- dist_fx(PUs = PUs, fauna = mmF, ofdates = OFCdates, cutoff = cutoff)
      }
      # Tidy up the final list
      FFdf <- do.call(rbind, FF)
      })
    
    
  # File name for the output
    lapply(FFdf, function(x){
      sgl <- x
      ngrd <- paste0(unlist(stringr::str_split(basename(output), "_"))[2:3], collapse = "_")
      ndate <- paste0(unlist(stringr::str_split(unlist(unique(sgl$dates)[1]), "-"))[1:2], collapse = "-")
      ffname <- paste(ngrd, ndate, unique(gsub("[./]", "", sgl$group)), sep = "_")
      saveRDS(sgl, paste0(output, ffname, paste("_cutoff", cutoff, sep = "-"), ".rds"))
    })
    return(FFdf)
}

# Focus on -> 2021-07; 2021-08
# SeaBirds
  # system.time(tt <- neardist_sim(pus = "input_layers/boundaries/PUs_NA_04km2.shp",
  #                                fsle_sf = "data_rout/dt_NA_allsat_madt_fsle_2021-08.rds",
  #                                fdata = "inputs_sb/sb_LSP_T",
  #                                cutoff = 0.75,
  #                                output = "outputs_sb/sb_LSP_T/"))
  
  # system.time(tt <- neardist_sim(pus = "/home/sandbox-sparc/NACECC-Initiative/input_layers/boundaries/PUs_NA_04km2.shp",
  #                                fsle_sf = "/home/sandbox-sparc/NACECC-Initiative/data_rout/dt_NA_allsat_madt_fsle_2021-08.rds",
  #                                fdata = "/home/sandbox-sparc/NACECC-Initiative/inputs_sb/sb_LSP_F",
  #                                cutoff = 0.75,
  #                                output = "/scratch/sparc/outputs_sb/sb_LSP_F/"))
  
  system.time(tt <- neardist_sim(pus = "/home/sandbox-sparc/NACECC-Initiative/input_layers/boundaries/PUs_NA_04km2.shp",
                                 fsle_sf = "/home/sandbox-sparc/NACECC-Initiative/data_rout/dt_NA_allsat_madt_fsle_2021-08.rds",
                                 fdata = "/home/sandbox-sparc/NACECC-Initiative/inputs_sb/sb_LSP_T",
                                 cutoff = 0.75,
                                 output = "/scratch/sparc/outputs_sb/sb_LSP_T/"))
  
