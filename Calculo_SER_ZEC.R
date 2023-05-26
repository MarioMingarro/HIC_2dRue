library(readr)
library(tidyverse)
library(foreign)

datos <- foreign::read.dbf("A:/2dRUE_LIC/Data_2dRUE_area_estudio.dbf")
datos$lic_name <- as.character(datos$lic_name)
datos$CCAA <- as.character(datos$CCAA)
Encoding(datos$lic_name) <- "UTF-8"
Encoding(datos$CCAA) <- "UTF-8"



#Calculo del indice SER
# Con muestreo aleatorio dentro ZEC
Nombres <- unique(datos$lic_name)
Nombres <- Nombres[-34]

SER_LIC <- data.frame(Nombre = character(),
                      SER = numeric(),
                      ABR = numeric(),
                      BAS = numeric(),
                      MDEG = numeric(),
                      DEG = numeric(),
                      PBB = numeric(),
                      PAB = numeric(),
                      SMAD = numeric(),
                      MAD = numeric(),
                      REF = numeric(),
                      AAR = numeric())


for (i in 1:length(Nombres)){
  LIC <- datos %>% 
    filter(lic_name == Nombres[i])
  
  SER_1 <- data.frame(Nombre = "a",
                      SER = 2,
                      ABR = 2,
                      BAS = 2,
                      MDEG =2,
                      DEG = 2,
                      PBB = 2,
                      PAB = 2,
                      SMAD =2,
                      MAD = 2,
                      REF = 2,
                      AAR = 2)
  
  porcentaje_por_valor <- prop.table(table(LIC$gridcode)) * 100
  
  # Obtener el número de columnas en porcentaje_por_valor
  num_columnas <- length(porcentaje_por_valor)
  
  # Rellenar con ceros las columnas faltantes en SER_1
  SER_1$ABR  <- ifelse(num_columnas >= 1, porcentaje_por_valor[1], 0)
  SER_1$BAS  <- ifelse(num_columnas >= 2, porcentaje_por_valor[2], 0)
  SER_1$MDEG <- ifelse(num_columnas >= 3, porcentaje_por_valor[3], 0)
  SER_1$DEG  <- ifelse(num_columnas >= 4, porcentaje_por_valor[4], 0)
  SER_1$PBB  <- ifelse(num_columnas >= 5, porcentaje_por_valor[5], 0)
  SER_1$PAB  <- ifelse(num_columnas >= 6, porcentaje_por_valor[6], 0)
  SER_1$SMAD <- ifelse(num_columnas >= 7, porcentaje_por_valor[7], 0)
  SER_1$MAD  <- ifelse(num_columnas >= 8, porcentaje_por_valor[8], 0)
  SER_1$REF  <- ifelse(num_columnas >= 9, porcentaje_por_valor[9], 0)
  SER_1$AAR  <- ifelse(num_columnas >= 10, porcentaje_por_valor[10], 0)
  
  SR_b <- sum(replace(porcentaje_por_valor[6:10],is.na(porcentaje_por_valor[6:10]),0))
  SR_a <- sum(replace(porcentaje_por_valor[3:5],is.na(porcentaje_por_valor[3:5]),0))
  
  SER_2 <- (SR_b-SR_a)/(SR_b+SR_a)
  SER_1$SER <- SER_2
  SER_1$lic_name <- Nombres[i]
  SER_LIC <- rbind(SER_LIC, SER_1)
}

kk <- datos %>% group_by(lic_name, lic_code) %>%
  summarise(n = n()) 

SER_LIC <- left_join(SER_LIC, kk, by = "lic_name")

writexl::write_xlsx(SER_LIC, "A:/2dRUE_LIC/RESULTADOS_LIC_SER.xlsx")
