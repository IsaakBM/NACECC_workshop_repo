# This code was written by Isaac Brito-Morales (ibrito@conservation.org)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

library(dplyr)
library(readr)

########################
####### Seabirds
########################
# General file
  sbG <- readRDS("data_raw/seabird_test/data_HMMclassified_Gull_2016_22.rds") %>% 
    as_tibble()
# Foraging behavior
  sbF_07 <- sbG %>% 
    dplyr::filter(datetime_utc %in% sbG$datetime_utc[stringr::str_detect(string = sbG$datetime_utc, pattern = "2021-07.*")]) %>% 
    dplyr::filter(states2from3 == "Foraging")
  sbF_08 <- sbG %>% 
    dplyr::filter(datetime_utc %in% sbG$datetime_utc[stringr::str_detect(string = sbG$datetime_utc, pattern = "2021-08.*")]) %>% 
    dplyr::filter(states2from3 == "Foraging")
# Transit behavior  
  sbT_07 <- sbG %>% 
    dplyr::filter(datetime_utc %in% sbG$datetime_utc[stringr::str_detect(string = sbG$datetime_utc, pattern = "2021-07.*")]) %>% 
    dplyr::filter(states2from3 == "Transit")
  sbT_08 <- sbG %>% 
    dplyr::filter(datetime_utc %in% sbG$datetime_utc[stringr::str_detect(string = sbG$datetime_utc, pattern = "2021-08.*")]) %>% 
    dplyr::filter(states2from3 == "Transit")
  