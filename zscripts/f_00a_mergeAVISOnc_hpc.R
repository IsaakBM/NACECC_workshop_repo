# This code was written by Isaac Brito-Morales (ibrito@conservation.org)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

merge_files <- function(ipath, opath1) {

  library(doParallel)
  library(parallel)
  library(stringr)
  options(useFancyQuotes = FALSE)

######################################################
####### Getting the path and directories for the files
######################################################
  # Establish the find bash command
    line1 <- paste(noquote("find"),
                   noquote(ipath),
                   "-type", "f", "-name",
                   dQuote("*.nc"),
                   "-exec", "ls",
                   "-l",
                   "{}")
    line2 <- paste0("\\", ";")
    line3 <- paste(line1, line2)
  # Getting a list of directories for every netCDF file
    dir_files <- system(line3, intern = TRUE)
    dir_nc <- strsplit(x = dir_files, split = " ")
    final_nc <- lapply(dir_nc, function(x){f1 <- tail(x, n = 1)}) # nolint
    final_nc <- unlist(final_nc) # nolint

######################################################
####### 
######################################################
  # Filtering by ocean month
    mF <- paste0(basename(ipath), sprintf("%02d", seq(1, 12, 1)))
    ls_dir <- vector(mode = "list", length = length(mF))
    for(i in seq_along(mF)) {
      ls_dir[[i]] <- final_nc[str_detect(string = basename(final_nc),
                                         pattern = mF[i]) == TRUE]
      }
  # Parallel loop [i could have done this at the above loop but just for fun... here... or maybe laziness]
    cl <- parallel::makeCluster(5)
    registerDoParallel(cl) # nolint
    foreach(i = 1:length(ls_dir), .packages = c("stringr")) %dopar% {
      # 
        f1 <- paste(unlist(strsplit(basename(ls_dir[[i]][1]), "_"))[c(1:5)], collapse = "_")
        f2 <- paste0(basename(ipath), "-", sprintf("%02d", 1:12)[i])
        f3 <- paste(f1, f2, sep = "_")
        output <- paste0(opath1, f3, ".nc", collapse = "")
      # CDO structure command: cmd input output
        input <- paste("cdo -L mergetime", paste0(ls_dir[[i]], collapse = " "), sep = " ") 
        system(paste(input, output, sep = " "))
    }
    parallel::stopCluster(cl)
}

# merge_files(ipath = "/home/sandbox-sparc/FSLEonSATA_SWIO_FINAL/2007/",
#             opath1 = "/scratch/sparc/2007/")

merge_files(ipath = "/home/sandbox-sparc/AVISO/2021/",
            opath1 = "/scratch/sparc/2021/")



