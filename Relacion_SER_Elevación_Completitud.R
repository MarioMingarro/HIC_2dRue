library(tidyverse)
library(readr)
library(ggpubr)

# CARGAR DATOS ----
LIC <- read_delim("A:/LIC_2dRUE/LIC_RUE/LIC_RUE_AREA25_CCAA_RBIO.txt", 
                  delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                  trim_ws = TRUE)

NP <- read_delim("A:/LIC_2dRUE/LIC_RUE/NO_RN2000_RUE_AREA25_CCAA_RBIO.txt", 
                 delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                 trim_ws = TRUE)

DEM_LIC <- read_delim("A:/LIC_2dRUE/DEM/DEM_LIC_2dRUE.txt", 
                      delim = ";", escape_double = FALSE, col_types = cols(Rowid_ = col_skip()), 
                      locale = locale(decimal_mark = ","), 
                      trim_ws = TRUE)

DEM_NP <- read_delim("A:/LIC_2dRUE/DEM/DEM_NO_RN2000_2dRUE.txt", 
                     delim = ";", escape_double = FALSE, col_types = cols(Rowid_ = col_skip()), 
                     locale = locale(decimal_mark = ","), 
                     trim_ws = TRUE)

COMPLETITUD_LIC <- read_delim("A:/LIC_2dRUE/rRUE/COMPLETITUD/FALTANTES_LIC_2dRUE.txt", 
                              delim = ";", escape_double = FALSE, col_types = cols(Rowid_ = col_skip()), 
                              locale = locale(decimal_mark = ","), 
                              trim_ws = TRUE)

COMPLETITUD_NP <- read_delim("A:/LIC_2dRUE/rRUE/COMPLETITUD/FALTANTES_NO_RN2000_2dRUE.txt", 
                             delim = ";", escape_double = FALSE, col_types = cols(Rowid_ = col_skip()), 
                             locale = locale(decimal_mark = ","), 
                             trim_ws = TRUE)

LIC <- left_join(LIC, DEM_LIC, by = c("FID" = "FID"))
LIC <- left_join(LIC, COMPLETITUD_LIC, by = c("FID" = "FID"))

NP <- left_join(NP, DEM_NP, by = c("FID" = "FID"))
NP <- left_join(NP, COMPLETITUD_NP, by = c("FID" = "FID"))

rm(DEM_LIC, DEM_NP,COMPLETITUD_LIC, COMPLETITUD_NP)


# INDICE SER ----
## LIC ----



# Crear un nuevo dataframe donde se almacenarán los resultados
SER_LIC <-  data.frame(SER = numeric(),
                       ELEVATION = numeric(),
                       COMPLETITUD = numeric())

for (j in 1:2000) {
  rand_LIC <- LIC[sample(nrow(LIC), size = 180), ]
  SER_1 <- data.frame(
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
  porcentaje_por_valor <- prop.table(table(rand_LIC$gridcode)) * 100
  
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
  
  SER_LIC_2 <- data.frame(SER = 2,
                          ELEVATION = 2,
                          COMPLETITUD = 2)
  
  SER_LIC_2$SER <- SER_2
  SER_LIC_2$ELEVATION <- mean(na.omit(rand_LIC$MEAN))
  SER_LIC_2$COMPLETITUD <- sum(na.omit(rand_LIC$SUM))
  SER_LIC <-  rbind(SER_LIC, SER_LIC_2)
  rm(porcentaje_por_valor, num_columnas, SR_b, SR_a, SER_1, SER_2, SER_LIC_2, rand_LIC)
}


a <- ggplot()+
  geom_point(data = SER_LIC, aes(SER_LIC$SER, SER_LIC$COMPLETITUD), alpha =.5)+
  geom_smooth(data = SER_LIC, aes(SER_LIC$SER, SER_LIC$COMPLETITUD), method = lm, se = FALSE)+
  labs(x= "SER", y = "Datos faltantes")+
  scale_x_continuous(limits = c(-0.1, 0.5), breaks = seq(-0.1, 0.5, 0.1))+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid.major = element_line(colour = "grey50")
  )

b <- ggplot()+
  geom_point(data = SER_LIC, aes(SER_LIC$SER, SER_LIC$ELEVATION), alpha =.5)+
  geom_smooth(data = SER_LIC, aes(SER_LIC$SER, SER_LIC$ELEVATION), method = lm, se = FALSE)+
  labs(x= "SER", y = "Elevación")+
  scale_x_continuous(limits = c(-0.1, 0.5), breaks = seq(-0.1, 0.5, 0.1))+
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50"),
    panel.grid.major = element_line(colour = "grey50")
  )

ggarrange(a, b, ncol = 1)
