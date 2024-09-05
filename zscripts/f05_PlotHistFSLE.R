# This code was written by Isaac Brito-Morales (ibrito@conservation.org)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

source("zscripts/z_helpFX.R")
source("zscripts/f04_MergeClean.R")

dfF <- ForagingTransit(indirFor = "outputs_sb/sb_LSP_F", 
                       indirTran = "outputs_sb/sb_LSP_T")

gg_dfF <- kernel_ggplot(input = dfF, 
                        xlabs = "Distance to high FSLE (km)", 
                        ylabs = "Density")
ggsave("figures/LSP_2021-07.png", plot = gg_dfF, width = 10, height = 6, dpi = 300, limitsize = FALSE)
