# REGBIO ----
library(tidyverse)
library(foreign)

datos <- foreign::read.dbf("D:/2dRUE_LIC/Data_2dRUE_area_estudio_REGBIO.dbf")
datos$lic_name <- as.character(datos$lic_name)
datos$CCAA <- as.character(datos$CCAA)
Encoding(datos$lic_name) <- "UTF-8"
Encoding(datos$CCAA) <- "UTF-8"
datos$REGBIO <- as.character(datos$REGBIO)
## LIC ----
# Con muestreo aleatorio dentro ZEC

# Filtrar los datos para seleccionar solo los registros donde la columna "Figura" es igual a "LIC"
LIC <- filter(datos, datos$Figura == "LIC")

# Crear un nuevo dataframe donde se almacenarán los resultados
SER_LIC <-  data.frame(
  REGBIO = character(),
  SER = numeric()
)

REGBIO <- unique(datos$REGBIO)

for (i in 1:length(REGBIO)) {
  filtrados <- filter(LIC, REGBIO == REGBIO[i])
  for (j in 1:16){
    rand_LIC <- filtrados[sample(nrow(filtrados), size = 193),]
    SER_1 <- data.frame(
      REGBIO = "a",
      SER = 2,
      ABR = 2,
      BAS = 2,
      MDEG = 2,
      DEG = 2,
      PBB = 2,
      PAB = 2,
      SMAD = 2,
      MAD = 2,
      REF = 2,
      AAR = 2
    )
    
    # Calcular el porcentaje de cada valor en la columna 'gridcode'
    porcentaje_por_valor <-
      prop.table(table(rand_LIC$gridcode)) * 100
    
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
    
    # Calcular los valores de SR_b y SR_a
    SR_b <- sum(replace(porcentaje_por_valor[6:10], is.na(porcentaje_por_valor[6:10]), 0))
    SR_a <- sum(replace(porcentaje_por_valor[3:5], is.na(porcentaje_por_valor[3:5]), 0))
    
    # Obtener el índice SER
    SER_2 <- (SR_b - SR_a) / (SR_b + SR_a)
    
    SER_LIC_2 <- data.frame(
      REGBIO = "a",
      SER = 2
    )
    
    SER_LIC_2$REGBIO <- REGBIO[i]
    SER_LIC_2$SER <- SER_2
    SER_LIC <-  rbind(SER_LIC, SER_LIC_2)
  }
}

SER_LIC %>%
  group_by(REGBIO) %>%
  summarise(area = mean(SER))


alp_ser <- filter(SER_LIC, SER_LIC$REGBIO == "ALP") 
alp_ser <- cbind("n" = rownames(alp_ser), alp_ser) 

atl_ser <- filter(SER_LIC, SER_LIC$REGBIO == "ATL")
atl_ser <- cbind("n" = rownames(atl_ser), atl_ser)

med_ser <- filter(SER_LIC, SER_LIC$REGBIO == "MED")
med_ser <- cbind("n" = rownames(med_ser), med_ser)

alp_ser_p <- ggplot(alp_ser, aes(x = n, y = SER)) +
  geom_point(alpha= .2, size = 2,  col = "blue")
atl_ser_p <- ggplot(atl_ser, aes(x = rownames(atl_ser), y = SER)) +
  geom_point(alpha= .2, size = 2,  col = "green")
med_ser_p <- ggplot(med_ser, aes(x = rownames(med_ser), y = SER)) +
  geom_point(alpha= .2, size = 2,  col = "red")

# SER promedio por CCAA
SER_LIC_REGBIO <- datos %>%
  group_by(datos$REGBIO) %>%
  summarise(area = sum(AREA_2)/193)
median(SER_LIC_REGBIO$area)

unique(filtrados$REGBIO)

filtrados %>% 
  summarise(area = sum(filtrados$AREA_2)) 

writexl::write_xlsx(SER_NP , "D:/2dRUE_LIC/SER_NP.xlsx")
## NP ----

# Con muestreo aleatorio fuera LIC

# Filtrar los datos para seleccionar solo los registros donde la columna "Figura" es igual a "LIC"
NP <- filter(datos, datos$Figura == "NP")

# Crear un nuevo dataframe donde se almacenarán los resultados
SER_NP <-  data.frame(
  REGBIO = character(),
  SER = numeric()
)



for (i in 1:length(REGBIO)) {
  filtrados <- filter(NP, REGBIO == REGBIO[i])
  for (j in 1:16){
    rand_NP <- filtrados[sample(nrow(filtrados), size = 193),]
    SER_1 <- data.frame(
      CCAA = "a",
      SER = 2,
      ABR = 2,
      BAS = 2,
      MDEG = 2,
      DEG = 2,
      PBB = 2,
      PAB = 2,
      SMAD = 2,
      MAD = 2,
      REF = 2,
      AAR = 2
    )
    
    # Calcular el porcentaje de cada valor en la columna 'gridcode'
    porcentaje_por_valor <-
      prop.table(table(rand_NP$gridcode)) * 100
    
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
    
    # Calcular los valores de SR_b y SR_a
    SR_b <- sum(replace(porcentaje_por_valor[6:10], is.na(porcentaje_por_valor[6:10]), 0))
    SR_a <- sum(replace(porcentaje_por_valor[3:5], is.na(porcentaje_por_valor[3:5]), 0))
    
    # Obtener el índice SER
    SER_2 <- (SR_b - SR_a) / (SR_b + SR_a)
    
    SER_NP_2 <- data.frame(
      REGBIO = "a",
      SER = 2
    )
    
    SER_NP_2$REGBIO <- REGBIO[i]
    SER_NP_2$SER <- SER_2
    SER_NP <-  rbind(SER_NP, SER_NP_2)
  }
}


# SER promedio por CCAA
SER_NP %>%
  group_by(REGBIO) %>%
  summarise(area = mean(SER)) 

writexl::write_xlsx(SER_NP_CCAA , "A:/2dRUE_LIC/SER_NP_CCAA.xlsx")