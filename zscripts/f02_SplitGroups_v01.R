# This code was written by Isaac Brito-Morales (ibrito@conservation.org)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

source("zscripts/z_inputFls_local.R")

f_split <- function(sps, outdir) {
# 
  library(sf)
  library(terra)
  library(raster)
  library(lubridate)
# 
  sps1 <- sps
  grp <- unique(sps1$birdID)
  grp2 <- unique(sps1$states2from3)
  
# Double for loop
  for(j in seq_along(grp)) {
  # 
    grp_sgl <- grp[j]
    df1 <- sps1 %>% 
      dplyr::filter(birdID == grp_sgl) %>% 
      dplyr::mutate(date = date(datetime_utc)) %>% 
      dplyr::select(x, y, birdID, datetime_utc, date, states3_hr, states2from3)
    
  # File name for the output
    ngrd <- unlist(stringr::str_split(basename(outdir), "_"))[2]
    ndate <- paste0(unlist(stringr::str_split(unlist(unique(df1$date)[1]), "-"))[1:2], collapse = "-")
    ffname <- paste(ngrd, gsub(" ", "", grp2), ndate, gsub("[./]", "", grp_sgl), sep = "_")
    saveRDS(df1, paste0(outdir, ffname, ".rds"))
    
  }
  
}

system.time(frw <- f_split(sps = sbT_07,
                           outdir = "inputs_sb/sb_LSP_T/"))
