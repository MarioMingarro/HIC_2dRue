Encinares o carrascales de Q. ilex subsp. ballota

library(palmerpenguins) # for 'penguins' dataset
library(ggstatsplot)
ggbetweenstats(ss, species, body_mass_g)


Data <- read.csv2("A:/GABRIEL/RESULTADOS/RESULTADOS_AN.txt", encoding="UTF-8")

Data$S_NAT3_c_1 <- str_replace(Data$S_NAT3_c_1 , ".*_", "")

# "Texto" 4 PROVINCIA
# "FIGURA" 13 FIGURA
# "NOMBRE" 14 Nombre PA
# "S_NAT3_c_1" 34 HIC
# "Estado"  28 RUE
# "AREA" 37 area

Data <- Data[,c(4,13,14,34,28,40)]
colnames(Data) <- c("PROVINCIA", "FIGURA", "NOMBRE", "HIC", "RUE", "AREA_m")
Data$AREA_km <- cbind(Data$AREA/1000000)

Data$FIGURA <- replace(Data$FIGURA,Data$FIGURA == "LIC", "RN2000")
Data$FIGURA <- replace(Data$FIGURA,Data$FIGURA == "ZEPA", "RN2000")

unique(Data$FIGURA)

kk <- dplyr::filter(Data, Data$HIC == "Encinares o carrascales de Q. ilex subsp. ballota")
kk <- dplyr::filter(kk, kk$RUE == "Degradado")

kk$RUE <- as.factor(kk$RUE)
ggbetweenstats(kk, FIGURA, AREA_km, plot.type = "boxviolin", type = "nonparametric")

kk <- dplyr::filter(kk, kk$)
hist(kk$AREA_m)
yy <- dplyr::filter(kk, kk$FIGURA == "RN2000")
yy <- yy$AREA_km
nn <- dplyr::filter(kk, kk$FIGURA == "No Protegido")
nn <- nn$AREA_km
wilcox.test(yy, nn)

