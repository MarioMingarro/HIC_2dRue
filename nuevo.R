# Carga los datos
library(readr)
library(tidyverse)
library(MASS)

datos <- read_delim("D:/GABRIEL/NUEVO/2dRUE_ZEC_NO_AP_25.txt", 
                            ";", escape_double = FALSE, 
                            locale = locale(decimal_mark = ",", grouping_mark = "."), 
                            trim_ws = TRUE)

kk <- datos %>% 
  filter(FIGURA== "ZEC")

kk2 <- datos %>% 
  filter(FIGURA== "NP")

var(kk$gridcode)
var(kk2$gridcode)

hist(kk$gridcode)
hist(kk2$gridcode)

anova
chi2 con los estados del RUE






kk <- datos %>% 
  filter(FIGURA== "ZEC") 

datos %>% 
  filter(FIGURA== "ZEC") %>% 
  filter(gridcode == "1") %>% 
  summarise(n())

porcentaje_por_valor <- prop.table(table(kk$gridcode)) * 100

alto <- mean(porcentaje_por_valor[6:10])
bajo <- mean(porcentaje_por_valor[3:5])

SER <- (alto-bajo)/(alto+bajo)


var(kk$gridcode)
# Crear un dataframe para almacenar los resultados
resultados <- data.frame()

# Realizar múltiples selecciones aleatorias del tamaño muestral
n_replicas <- 10000 # Número de replicas
for (i in 10:n_replicas) {
  muestra <- sample_n(kk, i, replace = TRUE)
  
  porcentaje_por_valor <- prop.table(table(muestra$gridcode)) * 100
  alto <- mean(porcentaje_por_valor[6:10])
  bajo <- mean(porcentaje_por_valor[3:5])
  
  SER <- (alto-bajo)/(alto+bajo)
  resultados <- rbind(resultados, data.frame(Replica = i, SER = SER))
}

porcentaje_por_valor <- prop.table(table(kk2$gridcode)) * 100
alto <- mean(porcentaje_por_valor[6:10])
bajo <- mean(porcentaje_por_valor[3:5])

SER_G <- (alto-bajo)/(alto+bajo)
# Generar el gráfico de caja y bigotes
ggplot(resultados, aes(x = Replica, y = SER)) +
  geom_point(alpha= .2, size = 0.5) +
  geom_hline(yintercept = SER_G, col = "red") +
  scale_x_continuous(breaks = c(10, 1000, 5000, 10000))+
  labs(x = "nº pixeles", y = "SER") +
  ggtitle("Estabilización del SER en Aleatorización del Tamaño Muestral") +
  theme_classic()

#################################

sd(SER_G)
