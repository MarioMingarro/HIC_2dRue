library(tidyverse)
library(foreign)

datos <- foreign::read.dbf("A:/2dRUE_LIC/Data_2dRUE_area_estudio_REGBIO.dbf")
datos$lic_name <- as.character(datos$lic_name)
datos$CCAA <- as.character(datos$CCAA)
Encoding(datos$lic_name) <- "UTF-8"
Encoding(datos$CCAA) <- "UTF-8"

# Área por CCAA
datos %>% 
  filter(Figura != "NP") %>% 
  group_by(CCAA) %>% 
  summarise(area = sum(AREA_RUE_2))

writexl::write_xlsx(datos %>% 
                      filter(Figura != "NP") %>% 
                      group_by(CCAA) %>% 
                      summarise(area = sum(AREA_RUE_2)), "A:/2dRUE_LIC/CCAA_areas.xlsx")


# Área por Region Biogeográfica
kk <- datos %>% 
  filter(Figura != "NP") %>% 
  group_by(REGBIO) %>% 
  summarise(area = sum(AREA_RUE_2))
writexl::write_xlsx(kk , "A:/2dRUE_LIC/REGBIO_areas.xlsx")

# Area promedio

datos %>% 
  filter(Figura != "NP") %>% 
  group_by(lic_name) %>% 
  summarise(area = sum(AREA_RUE_2)) %>% 
  summarise(area2 = mean(area))

# Area promedio LIC = 193 km2
# Varianza a 193 = 0.004311090



area_LIC <- datos %>% 
  filter(Figura != "NP") %>% 
  group_by(lic_name) %>% 
  summarise(area = sum(AREA_RUE_2))


writexl::write_xlsx(area_LIC , "A:/2dRUE_LIC/LIC_areas.xlsx")


datos %>% 
  filter(Figura != "NP") %>% 
  filter(CCAA == "Andalucía") %>% 
  group_by(lic_name) %>% 
  summarise(area = sum(AREA_RUE_2)) %>% 
  summarise(area2 = mean(area))


