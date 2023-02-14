library(tidyverse)
library(readxl)
library(reshape2)
library(raster)
library(stringr)

Data <- read.csv2("A:/GABRIEL/RESULTADOS/RESULTADOS.txt", encoding="UTF-8")

Data$S_NAT3_c_1 <- str_replace(Data$S_NAT3_c_1 , ".*_", "")

unique(Data$S_NAT3_c_1)

Data <- Data[,c(56,29,19,13,61)]
colnames(Data) <- c("PROVINCIA", "RN2000", "HIC", "RUE", "AREA")


unique(kk$variable)


"Estado"  13
"DESIG_ENG" 29
Texto 56
S_NAT3_c_1 19
"AREA_2" 61

unique(Data$RN2000)
unique(Data$HIC)

kk <- filter(Data, Data$HIC == "Bosques mixtos subatlánticos acidófilos")
ggplot(kk, aes(x = RUE, y = AREA/1000, fill = RN2000))+
  geom_boxplot()







RESULTADOS_ALMERIA <- read_excel("A:/GABRIEL/ALMERIA/RESULTADOS_ALMERIA.xlsx")
6 estado
58 area
61 codigo HIC
62 nombre HIC

RESULTADOS_ALMERIA <- RESULTADOS_ALMERIA[,c(61,62,6,58)]

kk <- melt(RESULTADOS_ALMERIA)

kk2 <- filter(kk, kk$Hoja1__Est == "Maduro")
kk2 <- filter(kk2, kk2$S_NAT2_cod_txt != "NA")

ggplot(kk2, aes(x = Hoja1__Est, y = value, fill = S_NAT2_cod_txt))+
  geom_boxplot()

unique(kk$S_NAT2_cod_txt)

kk2 <- filter(kk, kk$S_NAT2_cod_txt == "111MN_4_Sistemas ligados a los bosques esclerófilos mediterráneos")
ggplot(kk2, aes(x = Hoja1__Est, y = value, fill = Hoja1__Est))+
  geom_boxplot()
