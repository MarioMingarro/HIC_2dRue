library(tidyverse)
library(stringr)
library(extrafont)

# Análisis resumen para áreas y parches de THIC en la RN2000

Data <- read.csv2("D:/GABRIEL/RESULTADOS/NEW/THIC_AN.txt", encoding="UTF-8")

Data <- Data[,c(43, 49, 50)]
colnames(Data) <- c("THIC", "FIGURA", "AREA_m")
Data$AREA_km <- cbind(Data$AREA/1000000)

Areas_THIC <- Data %>% 
  group_by(THIC, FIGURA) %>% 
  summarise(media = sum(AREA_km)) %>% 
  pivot_wider(names_from = THIC, values_from = media)

writexl::write_xlsx(Areas_THIC, "D:/GABRIEL/RESULTADOS/NEW/Areas_THIC.xlsx")

parches <- Data %>% 
  group_by(THIC, FIGURA) %>% 
  summarise(media = n()) %>% 
  pivot_wider(names_from = THIC, values_from = media)

writexl::write_xlsx(parches, "D:/GABRIEL/RESULTADOS/NEW/parches_THIC.xlsx")


# Análisis resumen para áreas de THIC según el 2dRUE en la RN2000

Data <- read.csv2("D:/GABRIEL/RESULTADOS/NEW/THIC_RN_2dRUE_AN_2.txt", encoding="UTF-8")

Data <- Data[,c(44,50,54,55)]
colnames(Data) <- c("THIC", "FIGURA", "RUE", "AREA_m")
Data$AREA_km <- cbind(Data$AREA/1000000)


Areas_THIC_RUE <- Data %>% 
  group_by(THIC, FIGURA, RUE) %>% 
  summarise(media = sum(AREA_km)) %>% 
  pivot_wider(names_from = THIC, values_from = media)

writexl::write_xlsx(Areas_THIC_RUE, "D:/GABRIEL/RESULTADOS/NEW/Areas_THIC_RUE.xlsx")