# Carga los datos
library(readr)
library(tidyverse)
library(foreign)

datos <- foreign::read.dbf("D:/GABRIEL/NUEVO/salida.dbf")
datos <- read_delim("D:/GABRIEL/NUEVO/2dRUE_ZEC_NO_AP_25.txt", 
                            ";", escape_double = FALSE, 
                            locale = locale(decimal_mark = ",", grouping_mark = "."), 
                            trim_ws = TRUE)

Encoding(datos$Nombre) <- "UTF-8"

names <- unique(datos$Nombre)

names <- names[1:3]

SER <- data.frame(name = character(),
                  SER = numeric())

for (i in 1:length(names)){
  ZEC <- datos %>% 
    filter(Nombre == names[i])
  
  SER_1 <- data.frame(name = "a",
                    SER = 2)
  
  porcentaje_por_valor <- prop.table(table(ZEC$gridcode)) * 100
  
  SR_b <- sum(replace(porcentaje_por_valor[6:10],is.na(porcentaje_por_valor[6:10]),0))
  
  SR_a <- sum(replace(porcentaje_por_valor[3:5],is.na(porcentaje_por_valor[3:5]),0))
  SER_2 <- (SR_b-SR_a)/(SR_b+SR_a)
  SER_1$SER <- SER_2
  SER_1$name <- names[i]
  SER <- rbind(SER, SER_1)
}
i=4
#GIS 71 con superficie mayor a 10km2
kk <- SER %>% 
  filter(SER >= -0.99) 

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
  muestra <- sample_n(ZEC, i, replace = TRUE)
  
  porcentaje_por_valor <- prop.table(table(muestra$gridcode)) * 100
  SR_b <- sum(porcentaje_por_valor[6:10])
  SR_a <- sum(porcentaje_por_valor[3:5])
  
  SER <- (SR_b-SR_a)/(SR_b+SR_a)
  resultados <- rbind(resultados, data.frame(Replica = i, SER = SER))
}

porcentaje_por_valor <- prop.table(table(NP$gridcode)) * 100
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
  geom_point(data=resultados_2, aes(x = Replica_2, y = SER_2), alpha= .2, size = 0.5, col = "red")+
  geom_hline(yintercept = SER_G, col = "black") +
  scale_x_continuous(breaks = c(10, 1000, 5000, 10000))+
  labs(x = "nº pixeles", y = "SER") +
  ggtitle("Estabilización del SER con reemplazo (azul) y sin reemplazo (rojo)") +
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



