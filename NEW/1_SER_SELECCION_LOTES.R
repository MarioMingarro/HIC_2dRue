closeAllConnections()
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Functions.R")

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