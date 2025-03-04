library(tidyverse)
library(sf)

LIC <- st_read("C:/A_TRABAJO/A_GABRIEL/SER/2dRUE_LIC.shp")
LIC <- LIC %>% filter(LIC$CCAA %in% c("ASTURIAS", "REGION DE MURCIA", "CANTABRIA", "GALICIA"))

## LIC 

# Obtener nombres únicos de licencias
C <- unique(LIC$lic_name)

# Crear un nuevo dataframe donde se almacenarán los resultados
SER_LIC <- data.frame(
  LIC = character(),
  CCAA = character(),
  SER = numeric()
)

# Nombres de columnas de SER asociadas a los códigos de gridcode
columnas_SER <- c("ABR", "BAS", "MDEG", "DEG", "PBB", "PAB", "SMAD", "MAD", "REF", "AAR")
names(columnas_SER) <- 1:10  # Suponiendo gridcodes del 1 al 10

for (i in 1:length(C)) {
  # Filtrar datos
  filtrados <- dplyr::filter(LIC, LIC$lic_name == C[i])
  
  # Calcular el porcentaje de cada valor en la columna 'gridcode'
  porcentaje_por_valor <- prop.table(table(filtrados$gridcode)) * 100
  
  # Crear un dataframe SER inicializado en 0
  SER <- as.data.frame(matrix(0, nrow = 1, ncol = length(columnas_SER)))
  colnames(SER) <- columnas_SER
  
  # Asignar valores según gridcode
  for (gc in names(porcentaje_por_valor)) {
    if (gc %in% names(columnas_SER)) {
      columna <- columnas_SER[[gc]]
      SER[[columna]] <- porcentaje_por_valor[[gc]]
      rm(columna,gc)
    }
  }
  
  # Calcular SR_b y SR_a asegurando que los valores fuera de rango sean 0
  SR_b <- sum(SER[, c("PAB", "SMAD", "MAD", "REF", "AAR")], na.rm = TRUE)
  SR_a <- sum(SER[, c("MDEG", "DEG", "PBB")], na.rm = TRUE)
  
 
  # Crear dataframe de resultados
  SER_LIC_2 <- data.frame(
    LIC = C[i],
    CCAA = unique(filtrados$CCAA),
    SER = ifelse((SR_b + SR_a) == 0, 0, (SR_b - SR_a) / (SR_b + SR_a))
  )
  
  # Unir SER con SER_LIC_2
  SER_LIC_2 <- cbind(SER_LIC_2, SER)
  
  # Agregar a SER_LIC
  SER_LIC <- rbind(SER_LIC, SER_LIC_2)
  rm(SER_LIC_2, SER, porcentaje_por_valor, SR_a, SR_b, i, filtrados)
}

# Mostrar el resultado final
print(SER_LIC)
writexl::write_xlsx(SER_LIC, "C:/A_TRABAJO/A_GABRIEL/SER/LIC_SER.xlsx")
write.csv2(SER_LIC, "C:/A_TRABAJO/A_GABRIEL/SER/LIC_SER.")