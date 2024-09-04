# This code was written by Isaac Brito-Morales (ibrito@conservation.org)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# geom_density() computes a smooth kernel density estimate (KDE), which is a smoothed version of the histogram. 
# It gives you a sense of the distribution shape, but it can sometimes obscure or smooth out smaller peaks or variations 
# if they are not prominent enough.

source("zscripts/z_helpFX.R")
source("zscripts/f04_MergeClean.R")

dfF <- modelvsrealH(indirData = "outputs_sb/sb_LSP_F", 
                     indirModel = "outputs_sb/sb_LSP_T")

gg_dfF <- entropie_mmf(input = dfF, 
             xlabs = "Distance to high FSLE (km)", 
             ylabs = "Density")
ggsave("figures/z_testing/LSP_2021-07.png", plot = gg_dfF, width = 10, height = 6, dpi = 300, limitsize = FALSE)
