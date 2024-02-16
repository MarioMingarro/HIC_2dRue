closeAllConnections()
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Functions.R")



# Clases 2dRUE
# CCAA ----
C <- unique(NP$CCAA)
## Clases 2dRUE---- 
### NP 
NP_CCAA <-  data.frame()

for (i in 1:length(C)) {
  rand_LIC <- filter(NP, NP$CCAA == C[i])
  SER_1 <- data.frame(
    CCAA = "a",
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
  
  SER_1$CCAA<- C[i]
  NP_CCAA <-  rbind(NP_CCAA, SER_1)
}

## LIC 
LIC_CCAA <-  data.frame()

for (i in 1:length(C)) {
  rand_LIC <- filter(LIC, LIC$CCAA == C[i])
  SER_1 <- data.frame(
    CCAA = "a",
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
  
  SER_1$CCAA<- C[i]
  LIC_CCAA <-  rbind(LIC_CCAA, SER_1)
}

LIC_CCAA <- mutate(LIC_CCAA, CCAA_C =
                     case_when(
                       LIC_CCAA$CCAA == "Galicia"                     ~ "GA", 
                       LIC_CCAA$CCAA == "Asturias, Principado de"     ~ "AS",
                       LIC_CCAA$CCAA == "Cantabria"                   ~ "CN",
                       LIC_CCAA$CCAA == "PaÃ­s Vasco"                  ~ "PV",
                       LIC_CCAA$CCAA == "Comunidad Foral de Navarra"  ~ "NA",
                       LIC_CCAA$CCAA == "Castilla y LeÃ³n"             ~ "CL",
                       LIC_CCAA$CCAA == "AragÃ³n"                      ~ "AR",
                       LIC_CCAA$CCAA == "CataluÃ±a"                    ~ "CT",
                       LIC_CCAA$CCAA == "Rioja, La"                   ~ "LR",
                       LIC_CCAA$CCAA == "Castilla - La Mancha"        ~ "CM",
                       LIC_CCAA$CCAA == "Comunidad de Madrid"         ~ "MA",
                       LIC_CCAA$CCAA == "Comunitat Valenciana"        ~ "CV",
                       LIC_CCAA$CCAA == "Extremadura"                 ~ "EX",
                       LIC_CCAA$CCAA == "AndalucÃ­a"                   ~ "AN",
                       LIC_CCAA$CCAA == "RegiÃ³n de Murcia"            ~ "MU"))
NP_CCAA <- mutate(NP_CCAA, CCAA_C =
                    case_when(
                      NP_CCAA$CCAA == "Galicia"                     ~ "GA", 
                      NP_CCAA$CCAA == "Asturias, Principado de"     ~ "AS",
                      NP_CCAA$CCAA == "Cantabria"                   ~ "CN",
                      NP_CCAA$CCAA == "PaÃ­s Vasco"                  ~ "PV",
                      NP_CCAA$CCAA == "Comunidad Foral de Navarra"  ~ "NA",
                      NP_CCAA$CCAA == "Castilla y LeÃ³n"             ~ "CL",
                      NP_CCAA$CCAA == "AragÃ³n"                      ~ "AR",
                      NP_CCAA$CCAA == "CataluÃ±a"                    ~ "CT",
                      NP_CCAA$CCAA == "Rioja, La"                   ~ "LR",
                      NP_CCAA$CCAA == "Castilla - La Mancha"        ~ "CM",
                      NP_CCAA$CCAA == "Comunidad de Madrid"         ~ "MA",
                      NP_CCAA$CCAA == "Comunitat Valenciana"        ~ "CV",
                      NP_CCAA$CCAA == "Extremadura"                 ~ "EX",
                      NP_CCAA$CCAA == "AndalucÃ­a"                   ~ "AN",
                      NP_CCAA$CCAA == "RegiÃ³n de Murcia"            ~ "MU"))


# Density plot 2dRUE class --------
LIC_CCAA_R <- reshape2::melt(LIC_CCAA)
NP_CCAA_R <- reshape2::melt(NP_CCAA)

ggplot()+
  geom_point(data= LIC_CCAA_R, aes(x= variable, y = value, group = CCAA), col = "aquamarine3", size = 2, alpha =.8)+
  geom_line(data= LIC_CCAA_R, aes(x= variable, y = value, group = CCAA), col = "aquamarine3", alpha =.5)+
  geom_point(data= NP_CCAA_R, aes(x= variable, y = value, group = CCAA), col = "coral3", size = 2, alpha =.8)+
  geom_line(data= NP_CCAA_R, aes(x= variable, y = value, group = CCAA), col = "coral3", alpha =.5)+
  facet_wrap(~CCAA_C, ncol=3)+
  geom_vline(xintercept = 5, col = "red", alpha = .5, t)+
  labs(y = "%")+
  theme(strip.background = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        legend.position = "none",
        panel.grid.major.y = element_line(size = 0.1, linetype = 'dashed',
                                          colour = alpha("gray60",0.5)),
        panel.grid.major.x = element_line(size = 0.1, linetype = 'dashed',
                                          colour = alpha("gray60",0.5)),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = "mono", angle = 45, size = 10, hjust = 1),
        axis.text.y = element_text(family = "mono", size = 10),
        axis.title = element_text(family = "mono", angle = 90, size = 12),
        strip.text = element_text(family = "mono", size = 10))

# Distancias ----
distancias <- data.frame(
  CCAA = character(),
  DISTANCES = numeric())

for(i in 1:length(C)){
  kk <- data.frame(
    CCAA = "a",
    DISTANCES = 1)
  a <- filter(LIC_CCAA, LIC_CCAA$CCAA== C[i])
  b <- filter(NP_CCAA, NP_CCAA$CCAA == C[i])
  a <- as.matrix(a[,c(2:11)])
  b <- as.matrix(b[,c(2:11)])
  kk$CCAA <- C[i]
  kk$DISTANCES <- emd2d(a, b, xdist = 1, ydist = 1, dist="euclidean")
  distancias <- rbind(distancias, kk)
}

distancias_CCAA <- distancias

## Asimetria -----
asimetria <- data.frame(
  CCAA = character(),
  LIC = numeric(),
  NP = numeric())

for(i in 1:length(C)){
  kk <- data.frame(
    CCAA = "a",
    LIC = 1,
    NP = 1)
  a <- filter(LIC, LIC$CCAA == C[i])
  b <- filter(NP, NP$CCAA == C[i])
  aa <- a$gridcode
  bb <- b$gridcode
  kk$CCAA <- C[i]
  kk$LIC <- skew(c(aa))
  kk$NP <- skew(c(bb))
  asimetria <- rbind(asimetria, kk)
}
asimetria_CCAA <- asimetria

###.............................................................................###

## REGBIO----

C <- unique(NP$REGBIO)
## Clases 2dRUE---- 
### NP 
NP_REGBIO <-  data.frame()

for (i in 1:length(C)) {
  rand_LIC <- filter(NP, NP$REGBIO == C[i])
  SER_1 <- data.frame(
    REGBIO = "a",
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
  
  SER_1$REGBIO<- C[i]
  NP_REGBIO <-  rbind(NP_REGBIO, SER_1)
}

## LIC 
LIC_REGBIO <-  data.frame()

for (i in 1:length(C)) {
  rand_LIC <- filter(LIC, LIC$REGBIO == C[i])
  SER_1 <- data.frame(
    REGBIO = "a",
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
  
  SER_1$REGBIO<- C[i]
  LIC_REGBIO <-  rbind(LIC_REGBIO, SER_1)
}


# Density plot 2dRUE class --------
LIC_REGBIO_R <- reshape2::melt(LIC_REGBIO)
NP_REGBIO_R <- reshape2::melt(NP_REGBIO)

ggplot()+
  geom_point(data= LIC_REGBIO_R, aes(x= variable, y = value, group = REGBIO), col = "aquamarine3", size = 2, alpha =.8)+
  geom_line(data= LIC_REGBIO_R, aes(x= variable, y = value, group = REGBIO), col = "aquamarine3", alpha =.5)+
  geom_point(data= NP_REGBIO_R, aes(x= variable, y = value, group = REGBIO), col = "coral3", size = 2, alpha =.8)+
  geom_line(data= NP_REGBIO_R, aes(x= variable, y = value, group = REGBIO), col = "coral3", alpha =.5)+
  facet_wrap(~REGBIO, ncol=3)+
  geom_vline(xintercept = 5, col = "red", alpha = .5, t)+
  labs(y = "%")+
  theme(strip.background = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        legend.position = "none",
        panel.grid.major.y = element_line(size = 0.1, linetype = 'dashed',
                                          colour = alpha("gray60",0.5)),
        panel.grid.major.x = element_line(size = 0.1, linetype = 'dashed',
                                          colour = alpha("gray60",0.5)),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = "mono", angle = 45, size = 10, hjust = 1),
        axis.text.y = element_text(family = "mono", size = 10),
        axis.title = element_text(family = "mono", angle = 90, size = 12),
        strip.text = element_text(family = "mono", size = 10))

# Distancias ----
distancias <- data.frame(
  REGBIO = character(),
  DISTANCES = numeric())

for(i in 1:length(C)){
  kk <- data.frame(
    REGBIO = "a",
    DISTANCES = 1)
  a <- filter(LIC_REGBIO, LIC_REGBIO$REGBIO== C[i])
  b <- filter(NP_REGBIO, NP_REGBIO$REGBIO == C[i])
  a <- as.matrix(a[,c(2:11)])
  b <- as.matrix(b[,c(2:11)])
  kk$REGBIO <- C[i]
  kk$DISTANCES <- emd2d(a, b, xdist = 1, ydist = 1, dist="euclidean")
  distancias <- rbind(distancias, kk)
}
distancias_REGBIO <- distancias

## Asimetria -----
asimetria <- data.frame(
  REGBIO = character(),
  LIC = numeric(),
  NP = numeric())

for(i in 1:length(C)){
  kk <- data.frame(
    REGBIO = "a",
    LIC = 1,
    NP = 1)
  a <- filter(LIC, LIC$REGBIO == C[i])
  b <- filter(NP, NP$REGBIO == C[i])
  aa <- a$gridcode
  bb <- b$gridcode
  kk$REGBIO <- C[i]
  kk$LIC <- skew(c(aa))
  kk$NP <- skew(c(bb))
  asimetria <- rbind(asimetria, kk)
}
asimetria_REGBIO <- asimetria
###..................................................................................###

