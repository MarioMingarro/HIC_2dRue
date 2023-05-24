# Carga los datos
library(readr)
library(tidyverse)
library(foreign)

datos <- foreign::read.dbf("D:/GABRIEL/NUEVO_2/2dRUE_area_estudio_025.dbf")
datos$ZEC__Nombr <- as.character(datos$ZEC__Nombr)
Encoding(datos$ZEC__Nombr) <- "UTF-8"

# Calculo de area y n pixeles
n <- datos %>% group_by(datos$ZEC__Nombr) %>%
  summarise(mean = mean(AREA_I), n = n()) 
n <- n[-23,]
n <- n$mean

# Calculo del indice SER
# Con muestreo aleatorio dentro ZEC
Nombres <- unique(datos$ZEC__Nombr)

SER_ZEC <- data.frame(name = character(),
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

ZEC <- filter(datos, datos$ZEC__FIGUR == "ZEC")

for (i in 1:64){
  rand_ZEC <- ZEC[sample(nrow(ZEC), size=n[i]),]
  
  SER_1 <- data.frame(n = "a",
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
  
  porcentaje_por_valor <- prop.table(table(rand_ZEC$gridcode)) * 100
  
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
  SER_1$n <- n[i]
  SER_ZEC <- rbind(SER_ZEC, SER_1)
}

writexl::write_xlsx(SER_ZEC, "D:/GABRIEL/NUEVO_2//RESULTADOS/RESULTADOS_ZEC_SER.xlsx")

# No protegido


SER_NP <- data.frame(name = character(),
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

No_protegido <- filter(datos, datos$ZEC__FIGUR == "NP")

for (i in 1:64){
  rand_NP <- No_protegido[sample(nrow(No_protegido), size=n[i]),]
  
  SER_1 <- data.frame(n = "a",
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
  
  porcentaje_por_valor <- prop.table(table(rand_NP$gridcode)) * 100
  
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
  SER_1$n <- n[i]
  SER_NP <- rbind(SER_NP, SER_1)
}


writexl::write_xlsx(SER_NP, "D:/GABRIEL/NUEVO_2//RESULTADOS/RESULTADOS_NP_SER.xlsx")

resultados <- data.frame()

25278*0.20
5055.6/64

# Realizar múltiples selecciones aleatorias del tamaño muestral

for (i in 10:10000) {
  muestra <- sample_n(ZEC, i, replace = TRUE)
  
  porcentaje_por_valor <- prop.table(table(muestra$gridcode)) * 100
  SR_b <- sum(porcentaje_por_valor[6:10])
  SR_a <- sum(porcentaje_por_valor[3:5])
  
  SER <- (SR_b-SR_a)/(SR_b+SR_a)
  resultados <- rbind(resultados, data.frame(Replica = i, SER = SER))
}

ggplot(data= resultados, aes(x = Replica, y = SER))+
  geom_point(col = "darkolivegreen4")+
  geom_smooth(se = F, col = "red")


## GRáFICO -----

ggplot()+
  geom_point(data= SER_ZEC, aes(x = n, y = SER), col = "darkolivegreen4")+
  geom_smooth(data= SER_ZEC, aes(x = n, y = SER), se = F, col = "darkolivegreen4")+
  geom_point(data= SER_NP, aes(x = n, y = SER), col = "red")+
  geom_smooth(data= SER_NP, aes(x = n, y = SER), se = F, col = "red")
