packages.to.use <- c("tidyverse",
                     "readr",
                     "ggpubr",
                     "reshape2",
                     "readxl",
                     "nortest",
                     "foreign")

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
LIC <- foreign::read.dbf("D:/LIC_2dRUE/DATA/LIC_DATA.dbf")
NP <- foreign::read.dbf( "D:/LIC_2dRUE/DATA/NO_RN2000_DATA.dbf")