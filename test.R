library(tidyverse)
library(readxl)
library(reshape2)
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
