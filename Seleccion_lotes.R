# Carga los datos
library(tidyverse)
library(foreign)

datos <- foreign::read.dbf("A:/2dRUE_LIC/Data_2dRUE_area_estudio_REGBIO.dbf")
datos$lic_name <- as.character(datos$lic_name)
datos$CCAA <- as.character(datos$CCAA)
Encoding(datos$lic_name) <- "UTF-8"
Encoding(datos$CCAA) <- "UTF-8"

# LIC ----

# Calculo del indice SER
# Con muestreo aleatorio dentro LIC

# Filtrar los datos para seleccionar solo los registros donde la columna "Figura" es igual a "LIC"
LIC <- filter(datos, datos$Figura == "LIC")
LIC <- filter(datos, datos$Figura == "LIC" & datos$CCAA == "Aragón")

# Crear un nuevo dataframe donde se almacenarán los resultados
SER_LIC <-  data.frame(
  name = character(),
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

for (i in 10:5000) {
  rand_LIC <- LIC[sample(nrow(LIC), size = i),]
  
  SER_1 <- data.frame(
    n = "a",
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
  SER_1$SER <- SER_2
  SER_1$n <- i
  SER_LIC <- rbind(SER_LIC, SER_1)
  rm(porcentaje_por_valor, num_columnas, SR_b, SR_a, SER_1, SER_2, SER_LIC_2, rand_LIC)
}
mean(SER_LIC$SER)


# Gráfico nº de pixeles utilizados en adicción
library(ggpubr)

a <- ggplot(SER_LIC, aes(x = n, y = SER)) +
  geom_point(alpha= .2, size = 2, col = "blue")+
  geom_hline(yintercept = mean(SER_LIC$SER))+
  geom_vline(xintercept = 193, col = "black")

b <- ggplot(SER_LIC, aes(x = n, y = SER)) +
  geom_point(alpha= .2, size = 2, col = "darkgreen")+
  xlim(c(0, 3000))+
  geom_hline(yintercept = mean(SER_LIC$SER))+
  geom_vline(xintercept = 193, col = "black")

c <- ggplot(SER_LIC, aes(x = n, y = SER)) +
  geom_point(alpha= .2, size = 2, col = "red")+
  xlim(c(0, 1000))+
  geom_hline(yintercept = mean(SER_LIC$SER))+
  geom_vline(xintercept = 193, col = "black")

ggarrange(ggarrange(a, b, ncol = 2),
          c,
          nrow = 2)




# Estabilización de la varianza

Varianza <- data.frame(n = character(),
                       var = numeric())

for(j in 1:9000){
  Varianza2 <- data.frame(n = 0,
                         var = 0)
  kk <- SER_LIC[j:(j+9),]
  Varianza2$n <- kk[10, 1]
  Varianza2$var <- var(kk$SER)
  Varianza <- rbind(Varianza, Varianza2)
}


# Gráfico estabilizacion de la varianza de 10 en 10

a <- ggplot(Varianza, aes(x = n, y = var)) +
  geom_point(alpha= .2, size = 2, col = "blue")+
  geom_vline(xintercept = 193, col = "black")

b <- ggplot(Varianza, aes(x = n, y = var)) +
  geom_point(alpha= .2, size = 2, col = "darkgreen")+
  xlim(c(0, 3000))+
  geom_vline(xintercept = 193, col = "black")

c <- ggplot(Varianza, aes(x = n, y = var)) +
  geom_point(alpha= .2, size = 2, col = "red")+
  xlim(c(0, 1000))+
  geom_vline(xintercept = 193, col = "black")

ggarrange(ggarrange(a, b, ncol = 2),
          c,
          nrow = 2)

# NP ----

# Calculo del indice SER
# Con muestreo aleatorio dentro LIC

# Filtrar los datos para seleccionar solo los registros donde la columna "Figura" es igual a "LIC"
NP <- filter(datos, datos$Figura == "NP")

# Crear un nuevo dataframe donde se almacenarán los resultados
SER_NP <-  data.frame(
  name = character(),
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

for (i in 10:5000) {
  rand_NP <- NP[sample(nrow(NP), size = i),]
  
  SER_1 <- data.frame(
    n = "a",
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
  
  # Calcular los valores de SR_b y SR_a
  SR_b <- sum(replace(porcentaje_por_valor[6:10], is.na(porcentaje_por_valor[6:10]), 0))
  SR_a <- sum(replace(porcentaje_por_valor[3:5], is.na(porcentaje_por_valor[3:5]), 0))
  
  # Obtener el índice SER
  SER_2 <- (SR_b - SR_a) / (SR_b + SR_a)
  SER_1$SER <- SER_2
  SER_1$n <- i
  SER_NP <- rbind(SER_NP, SER_1)
}

# Gráfico nº de pixeles utilizados en adicción
library(ggpubr)

a <- ggplot(SER_NP, aes(x = n, y = SER)) +
  geom_point(alpha= .2, size = 2, col = "blue")+
  geom_vline(xintercept = 193, col = "black")

b <- ggplot(SER_NP, aes(x = n, y = SER)) +
  geom_point(alpha= .2, size = 2, col = "darkgreen")+
  xlim(c(0, 3000))+
  geom_vline(xintercept = 193, col = "black")

c <- ggplot(SER_NP, aes(x = n, y = SER)) +
  geom_point(alpha= .2, size = 2, col = "red")+
  xlim(c(0, 1000))+
  geom_vline(xintercept = 193, col = "black")

ggarrange(ggarrange(a, b, ncol = 2),
          c,
          nrow = 2)




# Estabilización de la varianza

Varianza <- data.frame(n = character(),
                       var = numeric())

for(j in 1:9000){
  Varianza2 <- data.frame(n = 0,
                          var = 0)
  kk <- SER_NP[j:(j+9),]
  Varianza2$n <- kk[10, 1]
  Varianza2$var <- var(kk$SER)
  Varianza <- rbind(Varianza, Varianza2)
}


# Gráfico estabilizacion de la varianza de 10 en 10

a <- ggplot(Varianza, aes(x = n, y = var)) +
  geom_point(alpha= .2, size = 2, col = "blue")+
  geom_vline(xintercept = 193, col = "black")

b <- ggplot(Varianza, aes(x = n, y = var)) +
  geom_point(alpha= .2, size = 2, col = "darkgreen")+
  xlim(c(0, 3000))+
  geom_vline(xintercept = 193, col = "black")

c <- ggplot(Varianza, aes(x = n, y = var)) +
  geom_point(alpha= .2, size = 2, col = "red")+
  xlim(c(0, 1000))+
  geom_vline(xintercept = 193, col = "black")

ggarrange(ggarrange(a, b, ncol = 2),
          c,
          nrow = 2)
