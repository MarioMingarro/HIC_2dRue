library(tidyverse)
library(readr)
library(ggpubr)
library(reshape2)
library(readxl)

# CARGAR DATOS ----
LIC <- read_delim("D:/LIC_2dRUE/LIC_RUE/LIC_RUE_AREA25_CCAA_RBIO_FOREST_HFI.txt", 
                  delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                  trim_ws = TRUE)

NP <- read_delim("D:/LIC_2dRUE/LIC_RUE/NO_RN2000_RUE_AREA25_CCAA_RBIO_FOREST_HFI.txt", 
                 delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                 trim_ws = TRUE)

DEM_LIC <- read_delim("D:/LIC_2dRUE/DEM/DEM_LIC_2dRUE.txt", 
           delim = ";", escape_double = FALSE, col_types = cols(Rowid_ = col_skip()), 
           locale = locale(decimal_mark = ","), 
           trim_ws = TRUE)

DEM_NP <- read_delim("D:/LIC_2dRUE/DEM/DEM_NO_RN2000_2dRUE.txt", 
                     delim = ";", escape_double = FALSE, col_types = cols(Rowid_ = col_skip()), 
                     locale = locale(decimal_mark = ","), 
                     trim_ws = TRUE)

AISLAMIENTO <- readxl::read_xlsx("E:/LIC_2dRUE/AISLAMIENTO/expansionIndex.xlsx")

LIC <- left_join(LIC, DEM_LIC, by = c("FID" = "FID"))

LIC <- left_join(LIC, AISLAMIENTO, by = c("NOMBRE" = "Name"))
LIC <- rename(LIC, "AIS" = "expansionIndexNorm")
NP <- left_join(NP, DEM_NP, by = c("FID" = "FID"))


LIC <- filter(LIC, LIC$NOMBRE %in% unique(AISLAMIENTO$Name))

rm(DEM_LIC, DEM_NP)


nombres_LIC <- as.data.frame(unique(LIC$NOMBRE)) 
nombres_LIC <- mutate(nombres_LIC, LIC=rep(1, nrow(nombres_LIC)))
nombres_AIS <- as.data.frame(unique(AISLAMIENTO$Name))
nombres_AIS <- mutate(nombres_AIS, AIS=rep(1, nrow(nombres_AIS)))
colnames(nombres_LIC) <- c("nombre", "LIC")
colnames(nombres_AIS) <- c("nombre", "AIS")
kk <- left_join(nombres_LIC,nombres_AIS)
sum(is.na(kk$AIS)) 
# SELECCION LOTES I ----

## ESTABILIZACION SER ----
# Calculo del indice SER

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

for (i in 10:1000) {
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
  rm(porcentaje_por_valor, num_columnas, SR_b, SR_a, SER_1, SER_2, SER_NP_2, rand_NP)
}

## ESTABILIZACION VARIANZA SER ----

Varianza_LIC <- data.frame(n = character(),
                       var = numeric())

for(j in 1:991){
  Varianza2 <- data.frame(n = 0,
                          var = 0)
  kk <- SER_LIC[j:(j+9),]
  Varianza2$n <- kk[10, 1]
  Varianza2$var <- var(kk$SER)
  Varianza_LIC <- rbind(Varianza_LIC, Varianza2)
}

Varianza_NP <- data.frame(n = character(),
                           var = numeric())

for(j in 1:991){
  Varianza2 <- data.frame(n = 0,
                          var = 0)
  kk <- SER_NP[j:(j+9),]
  Varianza2$n <- kk[10, 1]
  Varianza2$var <- var(kk$SER)
  Varianza_NP <- rbind(Varianza_NP, Varianza2)
}

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


ggplot() +
  geom_point(data = SER_LIC, aes(x = n, y = SER),alpha= .2, size = 2, col = "aquamarine3")+
  geom_point(data = SER_NP, aes(x = n, y = SER),alpha= .2, size = 2, col = "coral3")+
  geom_hline(yintercept = mean(SER_LIC$SER),color="aquamarine3", linetype="dashed", size = 1)+
  geom_hline(yintercept = mean(SER_NP$SER),color="coral3", linetype="dashed", size = 1)+
  geom_vline(xintercept = 190, col = "black", size = 1)+
  geom_segment(aes(x = 600, y = -0.4, xend = 700, yend = -0.4), size = 1)+
  geom_text(data=data.frame(x = 800,y = -0.4,label = "Tamaño promedio LIC"), 
            aes( x=x, y=y, label=label),
            color="black", 
            size=3 , angle=0, fontface="bold" )+
  geom_segment(aes(x = 600, y = -0.5, xend = 700, yend = -0.5), color="aquamarine3", linetype="dashed", size = 1)+
  geom_text(data=data.frame(x = 800,y = -0.5,label = "SER promedio LIC España"), 
            aes( x=x, y=y, label=label),
            color="black", 
            size=3 , angle=0, fontface="bold" )+
  geom_segment(aes(x = 600, y = -0.6, xend = 700, yend = -0.6), color="coral3", linetype="dashed", size = 1)+
  geom_text(data=data.frame(x = 800,y = -0.6,label = "SER promedio NP España"), 
            aes( x=x, y=y, label=label),
            color="black", 
            size=3 , angle=0, fontface="bold")+
  labs(x= "Number of pixel")+
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


ggplot() +
  geom_point(data = Varianza_LIC, aes(x = n, y = var),alpha= .2, size = 2, col = "aquamarine3")+
  geom_point(data = Varianza_NP, aes(x = n, y = var),alpha= .2, size = 2, col = "coral3")+
  geom_vline(xintercept = 190, col = "black", size = 1)+
  geom_segment(aes(x = 600, y = 0.15, xend = 700, yend = 0.15), size = 1)+
  geom_text(data=data.frame(x = 800,y = 0.15,label = "Tamaño promedio LIC"), 
            aes( x=x, y=y, label=label),
            color="black", 
            size=3 , angle=0, fontface="bold" )+
  geom_segment(aes(x = 600, y = 0.14, xend = 700, yend = 0.14), color="aquamarine3", linetype="dashed", size = 1)+
  geom_text(data=data.frame(x = 800,y = 0.14,label = "SER promedio LIC España"), 
            aes( x=x, y=y, label=label),
            color="black", 
            size=3 , angle=0, fontface="bold" )+
  geom_segment(aes(x = 600, y = 0.13, xend = 700, yend = 0.13), color="coral3", linetype="dashed", size = 1)+
  geom_text(data=data.frame(x = 800,y = 0.13,label = "SER promedio NP España"), 
            aes( x=x, y=y, label=label),
            color="black", 
            size=3 , angle=0, fontface="bold")+
  labs(x= "Number of pixel")+
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


ggplot(Varianza, aes(x = n, y = var)) +
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
  labs(y = "SER variance", x= "Number of pixel")+
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


ggarrange(a, b)

n <- ((mean(AREA_CCAA$area_CCAA)*50)/100)/mean(AREA_LIC$area_LIC)
#m = 20
#n = 180

# INDICE SER ----
# ESPAÑA ----
## LIC ----

# Crear un nuevo dataframe donde se almacenarán los resultados

SER_LIC <-  data.frame(
    SER = numeric(),
    ELEVATION = numeric(),
    TCD = numeric(),
    HFI = numeric())


for (j in 1:1000) {
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
                          TCD = 2,
                          HFI = 2)
  
  SER_LIC_2$SER <- SER_2
  SER_LIC_2$ELEVATION <- mean(na.omit(rand_LIC$ELEVATION))
  SER_LIC_2$TCD <- mean(na.omit(rand_LIC$TCD))
  SER_LIC_2$HFI <- mean(na.omit(rand_LIC$HFI))
  SER_LIC <-  rbind(SER_LIC, SER_LIC_2)
  rm(porcentaje_por_valor, num_columnas, SR_b, SR_a, SER_1, SER_2, SER_LIC_2, rand_LIC)
}




## NP ----

# Crear un nuevo dataframe donde se almacenarán los resultados
SER_NP <-  data.frame(
  SER = numeric(),
  ELEVATION = numeric(),
  TCD = numeric(),
  HFI = numeric())


for (j in 1:1000) {
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
                          FOREST = 2)
  
  SER_NP_2$SER <- SER_2
  SER_NP_2$ELEVATION <- mean(na.omit(rand_NP$ELEVATION))
  SER_NP_2$TCD <- mean(na.omit(rand_NP$TCD))
  SER_NP_2$HFI <- mean(na.omit(rand_NP$HFI))
  SER_NP <-  rbind(SER_NP, SER_NP_2)
  rm(porcentaje_por_valor, num_columnas, SR_b, SR_a, SER_1, SER_2, SER_NP_2, rand_NP)
}

  
# COMPARACION SER ----
kk <- as.data.frame(cbind("ID" = as.numeric(rownames(SER_LIC)), "LIC" = as.numeric(SER_LIC$HFI), "NP" = SER_NP$HFI))


kk2 <- melt(kk[, c(2, 3)])

# Gráfico resultados

ggplot()+
  geom_boxplot(data= kk2, aes(y = value, x = factor(variable), col = variable, fill = variable), 
               alpha =.5)+
  scale_color_manual(values=c("aquamarine3", "coral3"))+
  scale_fill_manual(values=c("aquamarine3", "coral3"))+
  labs(y = "SER")+
  theme(
    legend.position = "none",
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


ggplot()+
  geom_point(data=kk, aes(x= kk$ID, y = kk$LIC), color = "aquamarine3", alpha=.5)+
  geom_point(data=kk, aes(x= kk$ID, y = kk$NP), color= "coral3", alpha=.5)+
  geom_hline(yintercept = mean(kk$LIC), col = "aquamarine3", linetype="dashed", size=1)+
  geom_hline(yintercept = mean(kk$NP), col = "coral3", linetype="dashed", size=1)+
  labs(y = "SER")+
  ylim(-1,1)+
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

Varianza <- data.frame(n = character(),
                      LIC = numeric(),
                      NP = numeric())

for(j in 1:991){
  Varianza2 <- data.frame(n = 0,
                          LIC = 0,
                          NP = 0)
  kk <- SER_LIC[j:(j+9),]
  Varianza2$n <- j
  Varianza2$LIC <- var(kk$SER)
  kk <- SER_NP[j:(j+9),]
  Varianza2$n <- j
  Varianza2$NP <- var(kk$SER)
  Varianza <- rbind(Varianza, Varianza2)
}

ggplot() +
  geom_point(data = Varianza, aes(x = n, y = Varianza$LIC), alpha= .2, size = 2, col = "aquamarine3")+
  geom_point(data = Varianza, aes(x = n, y = Varianza$NP), alpha= .2, size = 2, col = "coral3")+
  geom_hline(yintercept = mean(Varianza$LIC), col = "aquamarine3", linetype="dashed", size=1)+
  geom_hline(yintercept = mean(Varianza$NP), col = "coral3", linetype="dashed", size=1)+
  labs(y = "SER variance")+
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


# Tablas
a <- SER_LIC %>%
  summarise(SER = mean(SER),
            elevacion = mean(ELEVATION), 
            forest = mean(FOREST)) 


b <- SER_NP_ESPAÑA <- SER_NP %>%
  summarise(SER = mean(SER),
            elevacion = mean(ELEVATION), 
            forest = mean(FOREST)) 

RESULT_SPAIN <- as.data.frame(cbind(a[,1], b[,1], a[,2], b[,2], a[,3], b[,3]))


colnames(RESULT_SPAIN) <- c("SER_LIC", "SER_NP", "ELEVATION_LIC", "ELEVATION_NP", "TDC_LIC", "TDC_NP")

writexl::write_xlsx(RESULT_SPAIN, "E:/LIC_2dRUE/RESULT/RESULT_SPAIN.xlsx")



hist(kk$NP)
shapiro.test(kk$LIC)
shapiro.test(kk$NP)

wilcox.test(kk$LIC, kk$NP, paired = T)



# CCAA ----
## LIC ----

# Crear un nuevo dataframe donde se almacenarán los resultados

C <- unique(LIC$CCAA)


SER_LIC_CCAA <-  data.frame(
  CCAA = character(),
  SER = numeric(),
  ELEVATION = numeric(),
  TCD = numeric(),
  HFI = numeric(),
  AIS = numeric())

for (i in 1:length(C)) {
  filtrados <- filter(LIC, LIC$CCAA == C[i])
<<<<<<< HEAD
  for (j in 1:1000){
    rand_LIC <- filtrados[sample(nrow(filtrados), size = 190),]
=======
  for (j in 1:1){
    rand_LIC <- filtrados #[sample(nrow(filtrados), size = 190),]
>>>>>>> 944b73dcc5d30528e6275392b659929e73ea69a5
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
      TCD = 2,
      HFI = 2,
      AIS = 2
    )
    
    SER_LIC_2$CCAA <- C[i]
    SER_LIC_2$SER <- SER_2
    SER_LIC_2$ELEVATION <- median(na.omit(rand_LIC$ELEVATION))
    SER_LIC_2$TCD <- median(na.omit(rand_LIC$TCD))
    SER_LIC_2$HFI <- median(na.omit(rand_LIC$HFI))
    SER_LIC_2$AIS <- median(na.omit(rand_LIC$AIS))
    
    SER_LIC_2 <-  cbind( SER_LIC_2, SER_1)
    SER_LIC_CCAA <-  rbind(SER_LIC_CCAA, SER_LIC_2)
    
  }
  
}
SER_LIC_CCAA <- SER_LIC_CCAA[, -c(6,7)]

## NP ----

# Crear un nuevo dataframe donde se almacenarán los resultados

SER_NP_CCAA <-  data.frame(
    CCAA = character(),
    SER = numeric(),
    ELEVATION = numeric(),
    TCD = numeric(),
    HFI = numeric())
  
  for (i in 1:length(C)) {
    filtrados <- filter(NP, NP$CCAA == C[i])
    for (j in 1:1){
      rand_NP <- filtrados #[sample(nrow(filtrados), size = 190),]
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
        TCD = 2,
        HFI = 2
      )
      
      SER_NP_2$CCAA <- C[i]
      SER_NP_2$SER <- SER_2
      SER_NP_2$ELEVATION <- mean(na.omit(rand_NP$ELEVATION))
      SER_NP_2$TCD <- mean(na.omit(rand_NP$TCD))
      SER_NP_2$HFI <- mean(na.omit(rand_NP$HFI))
      SER_NP_2 <-  cbind(SER_NP_2, SER_1)
      SER_NP_CCAA <-  rbind(SER_NP_CCAA, SER_NP_2)
    }
    
  }
SER_NP_CCAA <- SER_NP_CCAA[, -c(6,7)]


## COMPARACION SER CCAA ----

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
  
  a <- filter(SER_LIC_CCAA,  CCAA== paste0(C[i]))
  b <- filter(SER_NP_CCAA,  CCAA== paste0(C[i]))

  a <- a[,4]
  b <- b[,4]
  
  SER_CCAA<- as.data.frame(cbind(a, b))
  colnames(SER_CCAA) <- c("LIC", "NP")
  
  SER_CCAA_COMPARATION$CCAA <- C[i]
  SER_CCAA_COMPARATION$shapiro_LIC <- shapiro.test(SER_CCAA$LIC)$p.value
  SER_CCAA_COMPARATION$shapiro_NP <- shapiro.test(SER_CCAA$NP)$p.value
  SER_CCAA_COMPARATION$wilcox.test.pvalue <- wilcox.test(SER_CCAA$LIC, SER_CCAA$NP, paired = T)$p.value
  SER_CCAA_COMPARATION$wilcox.test.W <- wilcox.test(SER_CCAA$LIC, SER_CCAA$NP, paired = T)$statistic
  SER_CCAA_COMPARATION_FINAL <- rbind(SER_CCAA_COMPARATION_FINAL, SER_CCAA_COMPARATION)
}

# SER promedio LIC por CCAA
a <- SER_LIC_CCAA %>%
  group_by(CCAA = fct_inorder(CCAA)) %>%
  summarise(SER = mean(SER),
            elevacion = mean(ELEVATION), 
            TCD = median(TCD),
            HFI = median(HFI),
            AIS = median(AIS)) 
# SER promedio NP por CCAA
b <- SER_NP_CCAA %>%
  group_by(CCAA = fct_inorder(CCAA)) %>%
  summarise(SER = mean(SER),
            elevacion = mean(ELEVATION), 
            TCD = mean(TCD),
            HFI = mean(HFI))

# Tablas resultados
RESULT_CCAA <- cbind(a[,1], a[,2], b[,2], a[,3], b[,3], a[,4], b[,4], a[,5], b[,5])
colnames(RESULT_CCAA) <- c("CCAA", "SER_LIC", "SER_NP", 
                           "ELEVATION_LIC", "ELEVATION_NP", 
                           "TDC_LIC", "TDC_NP",
                           "HFI_LIC", "HFI_NP")

writexl::write_xlsx(RESULT_CCAA, "E:/LIC_2dRUE/RESULT/RESULT_CCAA.xlsx")

kk1 <- SER_LIC_CCAA[, c(1, 3)]
kk1 <- melt(kk1)

kk2 <- SER_NP_CCAA[, c(1, 3)]
kk2 <- melt(kk2)

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
  labs(y = "Elevation")+
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

# Correlaciones
library(corrplot)
library(RColorBrewer)
cor_LIC <- SER_LIC_CCAA[,2:6]
cor_LIC <- cor(cor_LIC)
corrplot(cor_LIC, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
# REGBIO ----
## LIC ----

# Crear un nuevo dataframe donde se almacenarán los resultados

  
C <- unique(LIC$REGBIO)


SER_LIC_REGBIO <-  data.frame(
  REGBIO = character(),
  SER = numeric(),
  ELEVATION = numeric(),
  TCD = numeric(),
  HFI = numeric())

for (i in 1:length(C)) {
  filtrados <- filter(LIC, LIC$REGBIO == C[i])
  for (j in 1:1000){
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
      TCD = 2,
      HFI = 2
    )
    
    SER_LIC_2$REGBIO <- C[i]
    SER_LIC_2$SER <- SER_2
    SER_LIC_2$ELEVATION <- mean(na.omit(rand_LIC$ELEVATION))
    SER_LIC_2$TCD <- mean(na.omit(rand_LIC$TCD))
    SER_LIC_2$HFI <- mean(na.omit(rand_LIC$HFI))
    SER_LIC_REGBIO <-  rbind(SER_LIC_REGBIO, SER_LIC_2)
  }
  
}



## NP ----

SER_NP_REGBIO <-  data.frame(
  REGBIO = character(),
  SER = numeric(),
  ELEVATION = numeric(),
  TCD = numeric(),
  HFI = numeric())

for (i in 1:length(C)) {
  filtrados <- filter(NP, NP$REGBIO == C[i])
  for (j in 1:1000){
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
      TCD = 2,
      HFI = 2
    )
    
    SER_NP_2$REGBIO <- C[i]
    SER_NP_2$SER <- SER_2
    SER_NP_2$ELEVATION <- mean(na.omit(rand_NP$ELEVATION))
    SER_NP_2$TCD <- mean(na.omit(rand_NP$TCD))
    SER_NP_2$HFI <- mean(na.omit(rand_NP$HFI))
    SER_NP_REGBIO <-  rbind(SER_NP_REGBIO, SER_NP_2)
  }
}

## COMPARACION SER REGBIO ----

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
  
  a <- filter(SER_LIC_REGBIO,  REGBIO== paste0(C[i]))
  b <- filter(SER_NP_REGBIO,  REGBIO== paste0(C[i]))
  
  a <- a[,4]
  b <- b[,4]
  
  SER_REGBIO<- as.data.frame(cbind(a, b))
  colnames(SER_REGBIO) <- c("LIC", "NP")
  
  SER_REGBIO_COMPARATION$REGBIO <- C[i]
  SER_REGBIO_COMPARATION$shapiro_LIC <- shapiro.test(SER_REGBIO$LIC)$p.value
  SER_REGBIO_COMPARATION$shapiro_NP <- shapiro.test(SER_REGBIO$NP)$p.value
  SER_REGBIO_COMPARATION$wilcox.test.pvalue <- wilcox.test(SER_REGBIO$LIC, SER_REGBIO$NP, paired = T)$p.value
  SER_REGBIO_COMPARATION$wilcox.test.W <- wilcox.test(SER_REGBIO$LIC, SER_REGBIO$NP, paired = T)$statistic
  SER_REGBIO_COMPARATION_FINAL <- rbind(SER_REGBIO_COMPARATION_FINAL, SER_REGBIO_COMPARATION)
}

# SER promedio LIC por REGBIO
a <- SER_LIC_REGBIO %>%
  group_by(REGBIO = fct_inorder(REGBIO)) %>%
  summarise(SER = mean(SER),
            elevacion = mean(ELEVATION), 
            TCD = mean(TCD),
            HFI = mean(HFI))
# SER promedio NP por REGBIO
b <- SER_NP_REGBIO %>%
  group_by(REGBIO = fct_inorder(REGBIO)) %>%
  summarise(SER = mean(SER),
            elevacion = mean(ELEVATION), 
            TCD = mean(TCD),
            HFI = mean(HFI))

# Tablas resultados
RESULT_REGBIO <- cbind(a[,1], a[,2], b[,2], a[,3], b[,3], a[,4], b[,4], a[,5], b[,5])
colnames(RESULT_REGBIO) <- c("REGBIO", "SER_LIC", "SER_NP", 
                             "ELEVATION_LIC", "ELEVATION_NP", 
                             "TDC_LIC", "TDC_NP", 
                             "HFI_LIC", "HFI_NP")

writexl::write_xlsx(RESULT_REGBIO, "E:/LIC_2dRUE/RESULT/RESULT_REGBIO.xlsx")

kk1 <- SER_LIC_REGBIO[, c(1, 3)]
kk1 <- melt(kk1)

kk2 <- SER_NP_REGBIO[, c(1, 3)]
kk2 <- melt(kk2)

# Gráfico resultados

ggplot()+
  geom_boxplot(data= kk1, aes(y = value, x = factor(REGBIO)), 
               fill = "aquamarine3", 
               colour ="aquamarine3", 
               alpha =.5)+
  geom_boxplot(data= kk2, aes(y = value, x = factor(REGBIO)), 
               fill = "coral3", 
               colour = "coral3",
               alpha =.5)+
  labs(y = "Elevation")+
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

# ALL LIC ----


nombres <- unique(LIC$NOMBRE)

SER_LIC <-  data.frame(
  LIC = character(),
  SER = numeric(),
  ELEVATION = numeric(),
  TCD = numeric(),
  HFI = numeric(),
  AIS = numeric())

for (i in 1:length(nombres)) {
    rand_LIC <- filter(LIC, LIC$NOMBRE == nombres[i])
    SER_1 <- data.frame(
      LIC = "a",
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
      LIC = "a",
      CCAA = "a",
      REGBIO = "a",
      SER = 2,
      ELEVATION = 2,
      TCD = 2,
      HFI = 2,
      AIS = 2
    )
    
    SER_LIC_2$LIC<- nombres[i]
    SER_LIC_2$CCAA<- rand_LIC[1, 9]
    SER_LIC_2$REGBIO<- rand_LIC[1, 10]
    SER_LIC_2$SER <- SER_2
    SER_LIC_2$ELEVATION <- mean(na.omit(rand_LIC$ELEVATION))
    SER_LIC_2$TCD <- median(na.omit(rand_LIC$TCD))
    SER_LIC_2$HFI <- median(na.omit(rand_LIC$HFI))
    SER_LIC_2$HFI <- median(na.omit(rand_LIC$AIS))
    SER_LIC_2 <-  cbind( SER_LIC_2, SER_1)
    SER_LIC <-  rbind(SER_LIC, SER_LIC_2)
}

SER_LIC <- SER_LIC[, -c(9,10)]

SER_LIC <- na.omit(SER_LIC)
SER_LIC_F <- filter(SER_LIC, SER_LIC$SER == NA)

# Correlaciones
sum(is.na(SER_LIC)) 
library(corrplot)
library(RColorBrewer)
cor_LIC <- SER_LIC[,4:8]
cor_LIC <- cor(cor_LIC)
corrplot(cor_LIC, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

max(SER_LIC_F$PAB)

writexl::write_xlsx(SER_LIC, "D:/LIC_2dRUE/RESULT/SER_LIC_ALL.xlsx")


modelo <- lm(SER ~ scale(ELEVATION) + scale(TCD) + scale(HFI) ,data = SER_LIC)
modelo <- lm(SER ~ ELEVATION + TCD + HFI ,data = SER_LIC)
summary(modelo)

library(corrplot)
library(RColorBrewer)
M <-cor(SER_LIC[,2:5])
corrplot(M, type="upper", method="number")


LIC <- read_delim("E:/LIC_2dRUE/LIC_RUE/LIC_RUE_AREA25_CCAA_RBIO_FOREST_HFI_PCP_TMED.txt", 
                  delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                  trim_ws = TRUE)
colnames(LIC) <- c("FID", "NOMBRE", "AREA", "PERIMETER", "SER", "ELEVATION","TCD","HFI", "PCP", "TMED")

M <-cor(LIC[,3:10])
corrplot(M, type="upper", method="number")

library(MuMIn)
options(na.action = "na.fail")
fm1 <- lm(SER ~  TCD  + AREA  + PCP+ TMED, data = LIC)

summary(fm1)
dd <- dredge(fm1, extra = list(
  "R^2", "*" = function(x) {
    s <- summary(x)
    c(Rsq = s$r.squared, adjRsq = s$adj.r.squared,
      F = s$fstatistic[[1]])
  }))
aa <- subset(dd, delta < 4)


fm1 <- lm(SER ~ ELEVATION + TCD +  AREA +  PCP + HFI+ PERIMETRO+TMED, data = LIC)
writexl::write_xlsx(aa, "E:/LIC_2dRUE/RESULT/modelos_SER_LIC.xlsx")


LIC <- read_delim("E:/LIC_2dRUE/LIC_RUE/LIC_RUE_CCAA.txt", 
                  delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                  trim_ws = TRUE)
SER_LIC %>% 
  group_by(SER_LIC$CCAA) %>% 
  summarise(SER = mean(SER))

SER_LIC_CCAA %>% 
  group_by(SER_LIC_CCAA$CCAA) %>% 
  summarise(SER = mean(SER))

LIC_CCAA <- SER_LIC_CCAA[,c(1, 6:15)]
NP_CCAA <- SER_NP_CCAA[,c(1, 6:15)]
LIC_CCAA <- reshape2::melt(LIC_CCAA)
NP_CCAA <- reshape2::melt(NP_CCAA)

ggplot()+
  geom_point(data= LIC_CCAA, aes(x= variable, y = value, group = CCAA, col = CCAA), size = 2, alpha =.5)+
  geom_line(data= LIC_CCAA, aes(x= variable, y = value, group = CCAA, col = CCAA))+
  geom_point(data= NP_CCAA, aes(x= variable, y = value, group = CCAA), size = 2, alpha =.3)+
  geom_line(data= NP_CCAA, aes(x= variable, y = value, group = CCAA), alpha =.3)+
  facet_wrap(~CCAA, ncol=3)+
  geom_vline(xintercept = 5, col = "red", alpha = .5, t)+
  labs(y = "%")+
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
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
        axis.title = element_text(family = "mono", angle = 90, size = 12))
  
LIC_CCAA <- as.data.frame(t(SER_LIC_CCAA[,c(1, 6:15)]))
colnames(LIC_CCAA) <- LIC_CCAA[1,]
LIC_CCAA <- LIC_CCAA[-1,]
NP_CCAA <- as.data.frame(t(SER_NP_CCAA[,c(1, 6:15)]))
colnames(NP_CCAA) <- NP_CCAA[1,]
NP_CCAA <- NP_CCAA[-1,]

kk <- as.data.frame(cbind(  "LIC" = LIC_CCAA[,15], "NP" =  NP_CCAA[,15]))
rownames(kk) <- rownames(LIC_CCAA)
kk <-  transform(kk, LIC = as.numeric(LIC),
                 NP = as.numeric(NP))

library(ggmosaic)
kk <- as.data.frame(cbind(  "LIC" = LIC_CCAA[,15], "NP" =  NP_CCAA[,15]))
ggplot()+
  geom_mosaic(data= kk, aes(x = product(kk$LIC, kk$NP)), show.legend = FALSE)

mosaicplot(kk)
kk <-  transform(kk, LIC = as.numeric(LIC),
                 NP = as.numeric(NP))
chisq.test(kk$LIC, kk$NP)

library(ggmosaic)#https://journal.r-project.org/articles/RJ-2023-013/
ggplot(data = flights) + 
  geom_mosaic(aes(x = product(do_you_recline, rude_to_recline), 
                  fill = do_you_recline),
              divider = c("vspine", "hbar"))
