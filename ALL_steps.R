library(tidyverse)
library(readr)
library(ggpubr)
library(reshape2)
library(readxl)

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

# SELECCION LOTES I ----

## ESTABILIZACION SER ----
# Calculo del indice SER

# Crear un nuevo dataframe donde se almacenarán los resultados
LIC <- filter(LIC, LIC$CCAA == "Andalucía")
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

for (i in 10:1000) {
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

# Gráfico nº de pixeles utilizados en adicción
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


## ESTABILIZACION VARIANZA SER ----

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
  geom_vline(xintercept = 190, col = "black")

b <- ggplot(Varianza, aes(x = n, y = var)) +
  geom_point(alpha= .2, size = 2, col = "darkgreen")+
  xlim(c(0, 3000))+
  geom_vline(xintercept = 190, col = "black")

c <- ggplot(Varianza, aes(x = n, y = var)) +
  geom_point(alpha= .2, size = 2, col = "red")+
  xlim(c(0, 1000))+
  geom_vline(xintercept = 190, col = "black")

ggarrange(ggarrange(a, b, ncol = 2),
          c,
          nrow = 2)


# SELECCION LOTES II ----


AREA_CCAA <- LIC %>% 
  group_by(LIC$CCAA) %>% 
  summarise(area_CCAA = sum(AREA_RUE))

AREA_LIC <- LIC %>% 
  group_by(LIC$NOMBRE) %>% 
  summarise(area_LIC = sum(AREA_RUE))

# Area promedio LIC España
mean(AREA_LIC$area_LIC)

annotation <- data.frame(
  x = 350,
  y = -0.6,
  label = "Tamaño promedio LIC"
)


a <- ggplot(SER_LIC, aes(x = n, y = SER)) +
  geom_point(alpha= .2, size = 2, col = "red")+
  xlim(c(0, 1000))+
  geom_hline(yintercept = mean(SER_LIC$SER),color="red", linetype="dashed")+
  geom_vline(xintercept = 190, col = "black")+
  labs(x= "Number of pixel")

b <- ggplot(Varianza, aes(x = n, y = var)) +
  geom_point(alpha= .2, size = 2, col = "red")+
  xlim(c(0, 1000))+
  geom_vline(xintercept = 190, col = "black")+
  geom_segment(aes(x = 450, y = 0.15, xend = 550, yend = 0.15))+
  geom_text(data=data.frame(x = 800,y = 0.15,label = "Tamaño promedio LIC"), 
                                                          aes( x=x, y=y, label=label),
                                                          color="black", 
                                                          size=3 , angle=0, fontface="bold" )+
  geom_segment(aes(x = 450, y = 0.13, xend = 550, yend = 0.13), color="red", linetype="dashed")+
  geom_text(data=data.frame(x = 800,y = 0.13,label = "SER promedio España"), 
            aes( x=x, y=y, label=label),
            color="black", 
            size=3 , angle=0, fontface="bold" )+
  labs(y = "SER variance", x= "Number of pixel")


ggarrange(a, b)

n <- ((mean(AREA_CCAA$area_CCAA)*50)/100)/mean(AREA_LIC$area_LIC)
#m = 20
#n = 180

# INDICE SER ----
# ESPAÑA ----
## LIC ----

# Crear un nuevo dataframe donde se almacenarán los resultados


SER_LIC_FINAL <-  data.frame(
  SER = seq(1,1,1),
  ELEVATION = seq(1,1,1),
  COMPLETITUD = seq(1,1,1))

for (k in 1:1000){
  SER_LIC <-  data.frame(
    SER = numeric(),
    ELEVATION = numeric(),
    COMPLETITUD = numeric())


for (j in 1:20) {
  rand_LIC <- LIC[sample(nrow(LIC), size = 190), ]
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

SER_LIC_ESPAÑA <- SER_LIC %>%
  summarise(SER = mean(SER),
            elevacion = mean(ELEVATION), 
            completitud = sum(COMPLETITUD)) 

colnames(SER_LIC_ESPAÑA) <- c(paste0("SER_LIC_", k), paste0("ELEVATION_", k), paste0("COMPLETITUD_", k))
SER_LIC_FINAL <- cbind(SER_LIC_FINAL, SER_LIC_ESPAÑA)
}

SER_LIC_ESPAÑA_FINAL <- SER_LIC_FINAL %>%
  select(contains( "COMPLE")) 

#SER_LIC_ESPAÑA_FINAL <- SER_LIC_ESPAÑA_FINAL[, -2]

## NP ----

# Crear un nuevo dataframe donde se almacenarán los resultados
SER_NP_FINAL <-  data.frame(
  SER = seq(1,1,1),
  ELEVATION = seq(1,1,1),
  COMPLETITUD = seq(1,1,1))

for (k in 1:1000){
  SER_NP <-  data.frame(
    SER = numeric(),
    ELEVATION = numeric(),
    COMPLETITUD = numeric())
  
  
  for (j in 1:20) {
    rand_NP <- NP[sample(nrow(NP), size = 190), ]
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
    
    SER_NP_2 <- data.frame(SER = 2,
                            ELEVATION = 2,
                            COMPLETITUD = 2)
    
    SER_NP_2$SER <- SER_2
    SER_NP_2$ELEVATION <- mean(na.omit(rand_NP$MEAN))
    SER_NP_2$COMPLETITUD <- sum(na.omit(rand_NP$SUM))
    SER_NP <-  rbind(SER_NP, SER_NP_2)
    rm(porcentaje_por_valor, num_columnas, SR_b, SR_a, SER_1, SER_2, SER_NP_2, rand_NP)
  }
  
  SER_NP_ESPAÑA <- SER_NP %>%
    summarise(SER = mean(SER),
              elevacion = mean(ELEVATION), 
              completitud = sum(COMPLETITUD)) 
  
  colnames(SER_NP_ESPAÑA) <- c(paste0("SER_NP_", k), paste0("ELEVATION_", k), paste0("COMPLETITUD_", k))
  SER_NP_FINAL <- cbind(SER_NP_FINAL, SER_NP_ESPAÑA)
}

SER_NP_ESPAÑA_FINAL <- SER_NP_FINAL %>%
  select(contains( "COMPLE"))

#COMPARACION SER
colnames(SER_LIC_ESPAÑA_FINAL) <- seq(1, length(SER_LIC_ESPAÑA_FINAL), 1)
colnames(SER_NP_ESPAÑA_FINAL) <- seq(1, length(SER_NP_ESPAÑA_FINAL), 1)
SER_ESPAÑA <- rbind(SER_LIC_ESPAÑA_FINAL, SER_NP_ESPAÑA_FINAL)
SER_ESPAÑA <- as.data.frame(t(SER_ESPAÑA))
colnames(SER_ESPAÑA) <- c("LIC", "NP")
SER_ESPAÑA <- SER_ESPAÑA[-1,]



hist(SER_ESPAÑA$NP)
shapiro.test(SER_ESPAÑA$LIC)
shapiro.test(SER_ESPAÑA$NP)

t.test(SER_ESPAÑA$LIC, SER_ESPAÑA$NP)

colMeans(SER_ESPAÑA)

# CCAA ----
## LIC ----

# Crear un nuevo dataframe donde se almacenarán los resultados
for (i in 1){
  

C <- unique(LIC$CCAA)

SER_LIC_FINAL <-  data.frame(
  CCAA = C,
  SER = seq(1,15,1),
  ELEVATION = seq(1,15,1),
  COMPLETITUD = seq(1,15,1))

for (k in 1:1000){
SER_LIC <-  data.frame(
  CCAA = character(),
  SER = numeric(),
  ELEVATION = numeric(),
  COMPLETITUD = numeric())

for (i in 1:length(C)) {
  filtrados <- filter(LIC, LIC$CCAA == C[i])
  for (j in 1:20){
    rand_LIC <- filtrados[sample(nrow(filtrados), size = 190),]
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
    
    SER_LIC_2 <- data.frame(
      CCAA = "a",
      SER = 2,
      ELEVATION = 2,
      COMPLETITUD = 2
    )
    
    SER_LIC_2$CCAA <- C[i]
    SER_LIC_2$SER <- SER_2
    SER_LIC_2$ELEVATION <- mean(na.omit(rand_LIC$MEAN))
    SER_LIC_2$COMPLETITUD <- sum(na.omit(rand_LIC$SUM))
    SER_LIC <-  rbind(SER_LIC, SER_LIC_2)
  }
  
}

# SER promedio por CCAA
SER_LIC_CCAA <- SER_LIC %>%
  group_by(CCAA = fct_inorder(CCAA)) %>%
  summarise(SER = mean(SER),
            elevacion = mean(ELEVATION), 
            completitud = sum(COMPLETITUD)) 
colnames(SER_LIC_CCAA) <- c("CCAA", paste0("SER_LIC_", k), paste0("ELEVATION_", k), paste0("COMPLETITUD_", k))
SER_LIC_FINAL <- left_join(SER_LIC_FINAL, SER_LIC_CCAA)
}

SER_LIC_CCAA_FINAL <- SER_LIC_FINAL %>%
  select(CCAA,contains( "SER")) 
SER_LIC_CCAA_FINAL <- SER_LIC_CCAA_FINAL[, -2]



## NP ----

# Crear un nuevo dataframe donde se almacenarán los resultados
SER_NP_FINAL <-  data.frame(
  CCAA = C,
  SER = seq(1,15,1),
  ELEVATION = seq(1,15,1),
  COMPLETITUD = seq(1,15,1))

for (k in 1:1000){
  SER_NP <-  data.frame(
    CCAA = character(),
    SER = numeric(),
    ELEVATION = numeric(),
    COMPLETITUD = numeric())
  
  for (i in 1:length(C)) {
    filtrados <- filter(NP, NP$CCAA == C[i])
    for (j in 1:20){
      rand_NP <- filtrados[sample(nrow(filtrados), size = 190),]
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
      
      SER_NP_2 <- data.frame(
        CCAA = "a",
        SER = 2,
        ELEVATION = 2,
        COMPLETITUD = 2
      )
      
      SER_NP_2$CCAA <- C[i]
      SER_NP_2$SER <- SER_2
      SER_NP_2$ELEVATION <- mean(na.omit(rand_NP$MEAN))
      SER_NP_2$COMPLETITUD <- sum(na.omit(rand_NP$SUM))
      SER_NP <-  rbind(SER_NP, SER_NP_2)
    }
    
  }
  
  # SER promedio por CCAA
  SER_NP_CCAA <- SER_NP %>%
    group_by(CCAA = fct_inorder(CCAA)) %>%
    summarise(SER = mean(SER),
              elevacion = mean(ELEVATION), 
              completitud = sum(COMPLETITUD)) 
  colnames(SER_NP_CCAA) <- c("CCAA", paste0("SER_NP_", k), paste0("ELEVATION_", k), paste0("COMPLETITUD_", k))
  SER_NP_FINAL <- left_join(SER_NP_FINAL, SER_NP_CCAA)
}

SER_NP_CCAA_FINAL <- SER_NP_FINAL %>%
  select(CCAA, contains("SER")) 

SER_NP_CCAA_FINAL <- SER_NP_CCAA_FINAL[, -2] 


}


## COMPARACION SER CCAA ----
# Seleccionar SER, ELEVATION o COMPLEITUD
SER_LIC_CCAA_FINAL <- SER_LIC_FINAL %>%
  select(CCAA,contains( "COMPL")) 
SER_LIC_CCAA_FINAL <- SER_LIC_CCAA_FINAL[, -2]

SER_NP_CCAA_FINAL <- SER_NP_FINAL %>%
  select(CCAA, contains("COMPL")) 

SER_NP_CCAA_FINAL <- SER_NP_CCAA_FINAL[, -2] 


# Test de wilcoxon 
SER_CCAA_COMPARATION_FINAL <-  data.frame(
  CCAA = character(),
  shapiro_LIC = numeric(),
  shapiro_NP = numeric(),
  wilcox.test.pvalue = numeric(),
  wilcox.test.W = numeric())

for( i in 1:length(C)){
  SER_CCAA_COMPARATION <-  data.frame(
    CCAA = "a",
    shapiro_LIC = 1,
    shapiro_NP = 1,
    wilcox.test.pvalue = 1,
    wilcox.test.W = 1)
  
  a <- SER_LIC_CCAA_FINAL[i,]
  b <- SER_NP_CCAA_FINAL[i,]
  colnames(a) <- seq(1, length(SER_LIC_CCAA_FINAL), 1)
  colnames(b) <- seq(1, length(SER_NP_CCAA_FINAL), 1)
  a <- a[,-1]
  b <- b[,-1]
  
  SER_CCAA<- rbind(a, b)
  SER_CCAA <- as.data.frame(t(SER_CCAA))
  colnames(SER_CCAA) <- c("LIC", "NP")
  
  SER_CCAA_COMPARATION$CCAA <- C[i]
  SER_CCAA_COMPARATION$shapiro_LIC <- shapiro.test(SER_CCAA$LIC)$p.value
  SER_CCAA_COMPARATION$shapiro_NP <- shapiro.test(SER_CCAA$NP)$p.value
  SER_CCAA_COMPARATION$wilcox.test.pvalue <- wilcox.test(SER_CCAA$LIC, SER_CCAA$NP, paired = T)$p.value
  SER_CCAA_COMPARATION$wilcox.test.W <- wilcox.test(SER_CCAA$LIC, SER_CCAA$NP, paired = T)$statistic
  SER_CCAA_COMPARATION_FINAL <- rbind(SER_CCAA_COMPARATION_FINAL, SER_CCAA_COMPARATION)
}


# Tablas resultados
kk1 <- SER_LIC_CCAA_FINAL
kk1 <- melt(kk1)

kk2 <- SER_NP_CCAA_FINAL
kk2 <- melt(kk2)


kk1 <- kk1 %>%
  group_by(CCAA) %>%
  summarise(LIC = mean(value)) 
kk2 <- kk2 %>%
  group_by(CCAA) %>%
  summarise(NP = mean(value)) 

SER_LIC_NP_CCAA <- left_join(kk1, kk2)

writexl::write_xlsx(SER_LIC_NP_CCAA, "A:/LIC_2dRUE/RESULT/COMPLE_CCAA.xlsx")

# Gráfico resultados

ggplot()+
  geom_boxplot(data= kk1, aes(y = value, x = factor(CCAA)), 
               fill = "aquamarine3", 
               colour ="aquamarine3", 
               alpha =.5)+
  geom_boxplot(data= kk2, aes(y = value, x = factor(CCAA)), 
               fill = "coral3", 
               colour = "coral3",
               alpha =.5)+
  labs(y = "SER")+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major.y = element_line(size = 0.1, linetype = 'dashed',
                                      colour = alpha("gray60",0.5)),
    panel.grid.major.x = element_line(size = 0.1, linetype = 'solid',
                                      colour = "gray60"),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(family = "mono", angle = 45, size = 10, hjust = 1),
    axis.text.y = element_text(family = "mono", size = 10),
    axis.title = element_text(family = "mono", angle = 90, size = 12)
  )

# REGBIO ----
## LIC ----

# Crear un nuevo dataframe donde se almacenarán los resultados
for (i in 1){
  
C <- unique(LIC$REGBIO)

SER_LIC_FINAL <-  data.frame(
  REGBIO = C,
  SER = seq(1,3,1),
  ELEVATION = seq(1,3,1),
  COMPLETITUD = seq(1,3,1))

for (k in 1:1000){
  SER_LIC <-  data.frame(
    REGBIO = character(),
    SER = numeric(),
    ELEVATION = numeric(),
    COMPLETITUD = numeric())
  
  for (i in 1:length(C)) {
    filtrados <- filter(LIC, LIC$REGBIO == C[i])
    for (j in 1:20){
      rand_LIC <- filtrados[sample(nrow(filtrados), size = 190),]
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
      
      SER_LIC_2 <- data.frame(
        REGBIO = "a",
        SER = 2,
        ELEVATION = 2,
        COMPLETITUD = 2
      )
      
      SER_LIC_2$REGBIO <- C[i]
      SER_LIC_2$SER <- SER_2
      SER_LIC_2$ELEVATION <- mean(na.omit(rand_LIC$MEAN))
      SER_LIC_2$COMPLETITUD <- sum(na.omit(rand_LIC$SUM))
      SER_LIC <-  rbind(SER_LIC, SER_LIC_2)
    }
    
  }
  
  # SER promedio por REGBIO
  SER_LIC_REGBIO <- SER_LIC %>%
    group_by(REGBIO = fct_inorder(REGBIO)) %>%
    summarise(SER = mean(SER),
              elevacion = mean(ELEVATION), 
              completitud = sum(COMPLETITUD)) 
  colnames(SER_LIC_REGBIO) <- c("REGBIO", paste0("SER_LIC_", k), paste0("ELEVATION_", k), paste0("COMPLETITUD_", k))
  SER_LIC_FINAL <- left_join(SER_LIC_FINAL, SER_LIC_REGBIO)
}

SER_LIC_REGBIO_FINAL <- SER_LIC_FINAL %>%
  select(REGBIO,contains( "SER")) 
SER_LIC_REGBIO_FINAL <- SER_LIC_REGBIO_FINAL[, -2]
rm(rand_LIC, SER_1, SER_LIC, SER_LIC_2, SER_LIC_REGBIO, SER_LIC_FINAL, filtrados)


## NP ----

# Crear un nuevo dataframe donde se almacenarán los resultados
SER_NP_FINAL <-  data.frame(
  REGBIO = C,
  SER = seq(1,3,1),
  ELEVATION = seq(1,3,1),
  COMPLETITUD = seq(1,3,1))

for (k in 1:1000){
  SER_NP <-  data.frame(
    REGBIO = character(),
    SER = numeric(),
    ELEVATION = numeric(),
    COMPLETITUD = numeric())
  
  for (i in 1:length(C)) {
    filtrados <- filter(NP, NP$REGBIO == C[i])
    for (j in 1:20){
      rand_NP <- filtrados[sample(nrow(filtrados), size = 190),]
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
      
      SER_NP_2 <- data.frame(
        REGBIO = "a",
        SER = 2,
        ELEVATION = 2,
        COMPLETITUD = 2
      )
      
      SER_NP_2$REGBIO <- C[i]
      SER_NP_2$SER <- SER_2
      SER_NP_2$ELEVATION <- mean(na.omit(rand_NP$MEAN))
      SER_NP_2$COMPLETITUD <- sum(na.omit(rand_NP$SUM))
      SER_NP <-  rbind(SER_NP, SER_NP_2)
    }
    
  }
  
  # SER promedio por REGBIO
  SER_NP_REGBIO <- SER_NP %>%
    group_by(REGBIO = fct_inorder(REGBIO)) %>%
    summarise(SER = mean(SER),
              elevacion = mean(ELEVATION), 
              completitud = sum(COMPLETITUD)) 
  colnames(SER_NP_REGBIO) <- c("REGBIO", paste0("SER_NP_", k), paste0("ELEVATION_", k), paste0("COMPLETITUD_", k))
  SER_NP_FINAL <- left_join(SER_NP_FINAL, SER_NP_REGBIO)
}

SER_NP_REGBIO_FINAL <- SER_NP_FINAL %>%
  select(REGBIO, contains("SER")) 

SER_NP_REGBIO_FINAL <- SER_NP_REGBIO_FINAL[, -2] 

rm(rand_NP, SER_1, SER_NP, SER_NP_2, SER_NP_REGBIO, SER_NP_FINAL, filtrados)
}
## COMPARACION SER REGBIO ----

# Seleccionar SER, ELEVATION o COMPLEITUD
SER_LIC_REGBIO_FINAL <- SER_LIC_FINAL %>%
  select(REGBIO,contains( "COMP")) 
SER_LIC_REGBIO_FINAL <- SER_LIC_REGBIO_FINAL[, -2]

SER_NP_REGBIO_FINAL <- SER_NP_FINAL %>%
  select(REGBIO, contains("COMP")) 

SER_NP_REGBIO_FINAL <- SER_NP_REGBIO_FINAL[, -2] 


# Test de wilcoxon 
SER_REGBIO_COMPARATION_FINAL <-  data.frame(
  REGBIO = character(),
  shapiro_LIC = numeric(),
  shapiro_NP = numeric(),
  wilcox.test.pvalue = numeric(),
  wilcox.test.W = numeric())

for( i in 1:length(C)){
  SER_REGBIO_COMPARATION <-  data.frame(
    REGBIO = "a",
    shapiro_LIC = 1,
    shapiro_NP = 1,
    wilcox.test.pvalue = 1,
    wilcox.test.W = 1)
  
  a <- SER_LIC_REGBIO_FINAL[i,]
  b <- SER_NP_REGBIO_FINAL[i,]
  colnames(a) <- seq(1, length(SER_LIC_REGBIO_FINAL), 1)
  colnames(b) <- seq(1, length(SER_NP_REGBIO_FINAL), 1)
  a <- a[,-1]
  b <- b[,-1]
  
  SER_REGBIO<- rbind(a, b)
  SER_REGBIO <- as.data.frame(t(SER_REGBIO))
  colnames(SER_REGBIO) <- c("LIC", "NP")
  
  SER_REGBIO_COMPARATION$REGBIO <- C[i]
  SER_REGBIO_COMPARATION$shapiro_LIC <- shapiro.test(SER_REGBIO$LIC)$p.value
  SER_REGBIO_COMPARATION$shapiro_NP <- shapiro.test(SER_REGBIO$NP)$p.value
  SER_REGBIO_COMPARATION$wilcox.test.pvalue <- wilcox.test(SER_REGBIO$LIC, SER_REGBIO$NP, paired = T)$p.value
  SER_REGBIO_COMPARATION$wilcox.test.W <- wilcox.test(SER_REGBIO$LIC, SER_REGBIO$NP, paired = T)$statistic
  SER_REGBIO_COMPARATION_FINAL <- rbind(SER_REGBIO_COMPARATION_FINAL, SER_REGBIO_COMPARATION)
}


# Tablas resultados
kk1 <- SER_LIC_REGBIO_FINAL
kk1 <- melt(kk1)

kk2 <- SER_NP_REGBIO_FINAL
kk2 <- melt(kk2)


kk1 <- kk1 %>%
  group_by(REGBIO) %>%
  summarise(LIC = mean(value)) 
kk2 <- kk2 %>%
  group_by(REGBIO) %>%
  summarise(NP = mean(value)) 

SER_LIC_NP_REGBIO <- left_join(kk1, kk2)

writexl::write_xlsx(SER_LIC_NP_REGBIO, "A:/LIC_2dRUE/RESULT/COMPLE_REGBIO.xlsx")

#Graficos
kk1 <- SER_LIC_REGBIO_FINAL
kk1 <- melt(kk1)

kk2 <- SER_NP_REGBIO_FINAL
kk2 <- melt(kk2)

ggplot()+
  geom_boxplot(data= kk1, aes(y = value, x = factor(REGBIO)), 
               fill = "aquamarine3", 
               colour ="aquamarine3", 
               alpha =.5)+
  geom_boxplot(data= kk2, aes(y = value, x = factor(REGBIO)), 
               fill = "coral3", 
               colour = "coral3",
               alpha =.5)+
  labs(y = "SER")+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major.y = element_line(size = 0.1, linetype = 'dashed',
                                      colour = alpha("gray60",0.5)),
    panel.grid.major.x = element_line(size = 0.1, linetype = 'solid',
                                      colour = "gray60"),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(family = "mono", angle = 45, size = 10, hjust = 1),
    axis.text.y = element_text(family = "mono", size = 10),
    axis.title = element_text(family = "mono", angle = 90, size = 12)
  )

