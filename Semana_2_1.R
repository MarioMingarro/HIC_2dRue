library(tidyverse)
library(stringr)

Data <- read.csv2("E:/GABRIEL/RESULTADOS/RESULTADOS_AN.txt", encoding="UTF-8")

Data$S_NAT3_c_1 <- str_replace(Data$S_NAT3_c_1 , ".*_", "")
Data <- Data[,c(4,13,14,34,28,25,40)]

colnames(Data) <- c("PROVINCIA", "FIGURA", "NOMBRE", "CHFE50", "RUE", "THIC", "AREA_m")
Data$FIGURA <- replace(Data$FIGURA, Data$FIGURA == "LIC", "RN2000")
Data$FIGURA <- replace(Data$FIGURA, Data$FIGURA == "ZEPA", "RN2000")
Data$AREA_km <- cbind(Data$AREA/1000000) 


THIC <- c(9120,9130,9150,9160,9180,9230,9240,9260,9320,9330,9340,9360,9370,9380,9430,9520,9530,9540,9550,9560,9570,9580)
Definicion_THIC <- c("Hayedos acidófilos atlánticos con sotobosque de Ilex y a veces de Taxus (Quercion robori-petraeae o Ilici-Fagenion)",
                "Hayedos del Asperulo-Fagetum",
                "Hayedos calcícolas medioeuropeos del Cephalanthero-Fagion",
                "Robledales pedunculados o albares subatlánticos y medioeuropeos del Carpinion betuli",
                "Bosques de laderas, desprendimientos o barrancos del Tilio-Acerion",
                "Robledales galaico-portugueses con Quercus robur y Quercus pyrenaica",
                "Robledales ibéricos de Quercus faginea y Quercus canariensis",
                "Bosques de Castanea sativa",
                "Bosques de Olea y Ceratonia",
                "Alcornocales de Quercus suber",
                "Encinares de Quercus ilex y Quercus rotundifolia",
                "Laurisilvas macaronésicas (Laurus, Ocotea)",
                "Palmerales de Phoenix",
                "Bosques de Ilex aquifolium",
                "Bosques montanos y subalpinos de Pinus uncinata (en sustratos yesosos o calcáreos)",
                "Abetales de Abies pinsapo",
                "Pinares (sud-) mediterráneos de pino negros endémicos",
                "Pinares mediterráneos de pinos mesogeanos endémicos",
                "Pinares endémicos canarios",
                "Bosques endémicos de Juniperus spp.",
                "Bosques de Tetraclinis articulata",
                "Bosques mediterráneos de Taxus baccata"
                )

THIC_code <- data.frame(THIC, Definicion_THIC)

Data <- left_join(x=Data, y=THIC_code, by = "THIC")

Data$RUE_CODE <- Data$RUE
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "Anomaias de bajo rendimiento", "1")
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "Basal"  , "2")
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "Muy degradado", "3")
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "Degradado", "4")
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "Productivo de baja biomasa", "5")
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "Productivo de alta biomasa", "6")
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "Submaduro", "7")
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "Maduro", "8")
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "De referencia", "9")
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "Anomalias de alto rendimiento", "10")



rm(Definicion_THIC)
rm(THIC)
# Wilcoson test

ii <- unique(Data$THIC)
jj <- c("Anomaias de bajo rendimiento","Basal","Muy degradado","Degradado","Productivo de baja biomasa",
         "Productivo de alta biomasa","Submaduro","Maduro","De referencia","Anomalias de alto rendimiento")

Tabla <- data.frame(THIC = character(),
                    RUE = character(),
                    p = numeric())

for (i in c(1,2,4,5,6,7,8,9,10,11)){
  sel <- dplyr::filter(Data, Data$THIC == ii[i])
  Tabla_1 <- data.frame(THIC= character(),
                        RUE = character(),
                        p = numeric())
  
  for (j in 1:length(RUE)){
    aa <- dplyr::filter(sel, sel$RUE == jj[j])
    try({
      WT <- wilcox.test(AREA_km ~ FIGURA, data = aa,
                      exact = FALSE)
      })
    Tabla_2 <- data.frame(THIC = "A",
                          RUE = "A",
                          p = 2)
    Tabla_2[1,1] <- ii[i]
    Tabla_2[1,2] <- jj[j]
    Tabla_2[1,3] <- WT$p.value
    Tabla_1 <- rbind(Tabla_1, Tabla_2)
    
  }
  Tabla <- rbind(Tabla, Tabla_1)
}
Tabla$p <-round(Tabla$p, 4)


Tabla_n <-  Tabla %>% 
  pivot_wider(
    names_from = THIC, 
    values_from = p
  )
write.csv(Tabla_n, "E:/GABRIEL/RESULTADOS/wilcoxon_THIC.csv")

ss <- Data[,c(2,10,6,8)]
ss$THIC <- as.character(ss$CHFE50)
tabla_kk <- ss %>%  
  pivot_wider(
    names_from = THIC, 
    values_from = AREA_km,
    values_fn = mean
  )
write.csv(tabla_kk, "E:/GABRIEL/RESULTADOS/areas_THIC.csv")

prop.table(Data$AREA_km)

CHFE50 <- unique(Data$CHFE50)
sel <- dplyr::filter(Data, Data$CHFE50 == CHFE50[1])
