closeAllConnections()
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Functions.R")

# INDICE SER ----
# ESPAÑA ----
## LIC ----

# Crear un nuevo dataframe donde se almacenarán los resultados

SER_LIC <-  data.frame(
  SER = numeric(),
  ELEVATION = numeric(),
  SLOPE = numeric(),
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
  SER_LIC_2$ELEVATION <- median(na.omit(rand_LIC$ELEV))
  SER_LIC_2$SLOPE <- median(na.omit(rand_LIC$SLO))
  SER_LIC_2$TCD <- median(na.omit(rand_LIC$TCD))
  SER_LIC_2$HFI <- median(na.omit(rand_LIC$HFI))
  SER_LIC <-  rbind(SER_LIC, SER_LIC_2)
  rm(porcentaje_por_valor, num_columnas, SR_b, SR_a, SER_1, SER_2, SER_LIC_2, rand_LIC)
}




## NP ----

# Crear un nuevo dataframe donde se almacenarán los resultados
SER_NP <-  data.frame(
  SER = numeric(),
  ELEVATION = numeric(),
  SLOPE = numeric(),
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
  
  SER_NP_2 <- data.frame(SER = 2)
  
  SER_NP_2$SER <- SER_2
  SER_NP_2$ELEVATION <- median(na.omit(rand_NP$ELE))
  SER_NP_2$SLOPE <- median(na.omit(rand_NP$SLO))
  SER_NP_2$TCD <- median(na.omit(rand_NP$TCD))
  SER_NP_2$HFI <- median(na.omit(rand_NP$HFI))
  SER_NP <-  rbind(SER_NP, SER_NP_2)
  rm(porcentaje_por_valor, num_columnas, SR_b, SR_a, SER_1, SER_2, SER_NP_2, rand_NP)
}


# COMPARACION SER ----
kk <- as.data.frame(cbind("ID" = as.numeric(rownames(SER_LIC)), "SCIs" = as.numeric(SER_LIC$SER), "UbN2000" = SER_NP$SER))


kk2 <- melt(kk[, c(2, 3)])

# Gráfico resultados

ggplot()+
  geom_boxplot(data= kk2, aes(y = value, x = factor(variable), col = variable, fill = variable), 
               alpha =.5)+
  scale_color_manual(values=c("aquamarine3", "coral3"))+
  scale_fill_manual(values=c("aquamarine3", "coral3"))+
  labs(y = "SER")+
  ylim(-0.5, 0.5)+
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major.y = element_line(size = 0.1, linetype = 'dashed',
                                      colour = alpha("gray60",0.5)),
    panel.grid.major.x = element_line(size = 0.1, linetype = 'solid',
                                      colour = alpha("gray60",0.5)),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(family = "mono", angle = 0, size = 12, hjust = 0.5),
    axis.text.y = element_text(family = "mono", size = 12),
    axis.title.y = element_text(family = "mono", angle = 90, size = 12)
  )

# Tablas
a <- SER_LIC %>%
  summarise(SER = median(SER),
            ELEVATION = median(ELEVATION), 
            SLOPE = median(SLOPE), 
            TCD = median(TCD),
            HFI = median(HFI)) 



b <- SER_NP %>%
  summarise(SER = median(SER),
            ELEVATION = median(ELEVATION), 
            SLOPE = median(SLOPE), 
            TCD = median(TCD),
            HFI = median(HFI)) 

RESULT_SPAIN <- as.data.frame(cbind(a[,1], b[,1], a[,2], b[,2], a[,3], b[,3],a[,4], b[,4],a[,5], b[,5]))


colnames(RESULT_SPAIN) <- c("SER_LIC", "SER_NP", 
                            "ELEVATION_LIC", "ELEVATION_NP", 
                            "SLOPE_LIC", "SLOPE_NP", 
                            "TDC_LIC", "TDC_NP",
                            "HFI_LIC", "HFI_NP")

writexl::write_xlsx(RESULT_SPAIN, "B:/A_GABRIEL/A_LIC_2dRUE/6_RESULTS/RESULT_SPAIN.xlsx")