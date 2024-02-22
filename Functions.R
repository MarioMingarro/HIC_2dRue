packages.to.use <- c("tidyverse",
                     "readr",
                     "ggpubr",
                     "reshape2",
                     "readxl",
                     "nortest",
                     "foreign",
                     "emdist",
                     "psych",
                     "corrplot",
                     "RColorBrewer")

packages.to.use <- unique(packages.to.use)

for(package in packages.to.use) {
  print(package)
  if( ! package %in% rownames(installed.packages()) ) { install.packages(package ) }
  if( ! package %in% rownames(installed.packages()) & package == "VoCC" ) { devtools::install_github("JorGarMol/VoCC", dependencies = TRUE) }
  if( ! package %in% rownames(installed.packages()) ) { stop("Error on package instalation") }
  suppressWarnings( library(package, character.only = TRUE) )
}
rm(packages.to.use,package)

# CARGAR DATOS ----
LIC <- foreign::read.dbf("B:/A_GABRIEL/A_LIC_2dRUE/5_DATA/LIC_DATA.dbf")
NP <- foreign::read.dbf( "B:/A_GABRIEL/A_LIC_2dRUE/5_DATA/NO_RN2000_DATA.dbf")

library(corrplot)
library(RColorBrewer)