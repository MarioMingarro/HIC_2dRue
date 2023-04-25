# Carga los datos
library(readr)
library(tidyverse)
library(foreign)

datos <- foreign::read.dbf("D:/GABRIEL/NUEVO/salida.dbf")
# datos <- read_delim("D:/GABRIEL/NUEVO/2dRUE_ZEC_NO_AP_25.txt", 
#                             ";", escape_double = FALSE, 
#                             locale = locale(decimal_mark = ",", grouping_mark = "."), 
#                             trim_ws = TRUE)

Encoding(datos$Nombre) <- "UTF-8"

# Calculo del indice SER
Nombres <- unique(datos$Nombre)

SER <- data.frame(name = character(),
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
  ZEC <- datos %>% 
    filter(Nombre == Nombres[i])
  
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
  
  porcentaje_por_valor <- prop.table(table(ZEC$gridcode)) * 100
 
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
  SER_1$Nombre <- Nombres[i]
  SER <- rbind(SER, SER_1)
}

writexl::write_xlsx(SER_NP, "D:/GABRIEL/NUEVO/RESULTADOS_NP_SER.xlsx")

#GIS 71 con superficie mayor a 10km2
kk <- SER %>% 
  filter(SER >= -0.99) 
kk <- kk[-1,]

#Relacion SER y area
aa <- datos[, c(5,7)]
aa <- distinct(aa)
aa <- aa[-1,]
kk <- left_join(kk, aa, by = "Nombre")

ggplot(kk, aes(x= AREA, y = SER))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE)
cor(kk$AREA, kk$SER)


# Calculo SER en zonas no porotegidas
mean(kk$AREA)
# Area 430
count(kk)
# n = 53
No_protegido <- filter(datos, datos$FIGURA == "NP")



SER_NP <- data.frame(i = numeric(),
                     SER = numeric())

for (i in 1:53){
  rand_No_protegido <- No_protegido[sample(nrow(No_protegido), size=500),]
  
  SER_1 <- data.frame(i = 2,
                      SER = 2)
  
  porcentaje_por_valor <- prop.table(table(rand_No_protegido$gridcode)) * 100
  
  SR_b <- sum(replace(porcentaje_por_valor[6:10],is.na(porcentaje_por_valor[6:10]),0))
  SR_a <- sum(replace(porcentaje_por_valor[3:5],is.na(porcentaje_por_valor[3:5]),0))
  
  
  SER_2 <- (SR_b-SR_a)/(SR_b+SR_a)
  SER_1$SER <- SER_2
  SER_1$i <- i
  SER_NP <- rbind(SER_NP, SER_1)
}

mean(SER_NP$SER)
-0.5708506



##  TEST

hist(kk$SER)
shapiro.test(kk$SER) #dist normal
shapiro.test(SER_NP$SER) #dist normal

t.test(kk$SER, SER_NP$SER)

nn <- kk$SER
nn <- cbind(nn, SER_NP$SER)
nn <- as.data.frame(nn)
colnames(nn) <- c("SAC", "NP")


nn <- reshape2::melt(nn)


ggplot(nn,
       aes(
         x = variable,
         y = value,
         fill = variable,
         colour = variable
       )) +
  geom_flat_violin(
    position = position_nudge(x = .1, y = 0),
    trim = FALSE,
    alpha = 0.5,
    colour = NA
  ) +
  geom_point(position = position_jitter(width = .2),
             size = 2,
             shape = 20) +
  geom_boxplot(
    outlier.shape = NA,
    alpha = .5,
    width = .1,
    colour = "black"
  ) +
  scale_y_continuous("SER", limits = c(-1,1))+
  scale_colour_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank()
  )

library(PupillometryR )

#######################################
#######################################
ZEC <- datos %>% 
  filter(FIGURA == "ZEC")

NP <- datos %>% 
  filter(FIGURA == "NP")

var(kk$gridcode)
var(kk2$gridcode)

hist(ZEC$gridcode)
hist(NP$gridcode)

#anova
#chi2 con los estados del RUE

porcentaje_por_valor <- prop.table(table(NP$gridcode)) * 100

SR_b <- sum(porcentaje_por_valor[6:10])
SR_a <- sum(porcentaje_por_valor[3:5])

SER <- (SR_b-SR_a)/(SR_b+SR_a)


# Crear un dataframe para almacenar los resultados
resultados <- data.frame()

# Realizar múltiples selecciones aleatorias del tamaño muestral

for (i in 10:10000) {
  muestra <- sample_n(No_protegido, i, replace = TRUE)
  
  porcentaje_por_valor <- prop.table(table(muestra$gridcode)) * 100
  SR_b <- sum(porcentaje_por_valor[6:10])
  SR_a <- sum(porcentaje_por_valor[3:5])
  
  SER <- (SR_b-SR_a)/(SR_b+SR_a)
  resultados <- rbind(resultados, data.frame(Replica = i, SER = SER))
}

porcentaje_por_valor <- prop.table(table(No_protegido$gridcode)) * 100
SR_b <- sum(porcentaje_por_valor[6:10])
SR_a <- sum(porcentaje_por_valor[3:5])

SER_G <- (SR_b-SR_a)/(SR_b+SR_a)


# Crear un dataframe para almacenar los resultados
resultados_2 <- data.frame()

# Realizar múltiples selecciones aleatorias del tamaño muestral

for (i in 10:10000) {
  muestra <- sample_n(ZEC, i, replace = FALSE)
  
  porcentaje_por_valor <- prop.table(table(muestra$gridcode)) * 100
  SR_b <- sum(porcentaje_por_valor[6:10])
  SR_a <- sum(porcentaje_por_valor[3:5])
  
  SER <- (SR_b-SR_a)/(SR_b+SR_a)
  resultados_2 <- rbind(resultados_2, data.frame(Replica_2 = i, SER_2 = SER))
}

# Generar el gráfico de caja y bigotes
ggplot(resultados, aes(x = Replica, y = SER)) +
  geom_point(alpha= .2, size = 0.5, col = "blue") +
  geom_hline(yintercept = c(SER_G, SER_G+0.1, SER_G-0.1), col = "black") +
  geom_hline(yintercept = SER_G,  col = "red") +
  geom_vline(xintercept = 500, col = "black") +
  scale_x_continuous(breaks = c(10, 500, 1000, 5000, 10000))+
  labs(x = "nº pixeles", y = "SER") +
  ggtitle("Estabilización del SER") +
  theme_classic()

#################################

resultados <- data.frame()

# Realizar múltiples selecciones aleatorias del tamaño muestral

for (i in 10:10000) {
  muestra <- sample_n(ZEC, i, replace = TRUE)
  
  varianza <- var(muestra$gridcode)
  
  resultados <- rbind(resultados, data.frame(Replica = i, Varianza = varianza))
}

varianza_g <- var(ZEC$gridcode)


ggplot(resultados, aes(x = Replica, y = Varianza)) +
  geom_point(alpha= .2, size = 0.5, col = "blue") +
  geom_hline(yintercept = varianza_g, col = "black") +
  scale_x_continuous(breaks = c(10, 1000, 5000, 10000))+
  labs(x = "nº pixeles", y = "Varianza") +
  ggtitle("Estabilización de varianza") +
  theme_classic()



