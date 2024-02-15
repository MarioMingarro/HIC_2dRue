closeAllConnections()
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Functions.R")

# REGBIO ----
## LIC ----

# Crear un nuevo dataframe donde se almacenarán los resultados


C <- unique(LIC$REGBIO)


SER_LIC_REGBIO <-  data.frame(
  REGBIO = character(),
  SER = numeric(),
  ELEVATION = numeric(),
  SLOPE = numeric(),
  TCD = numeric(),
  HFI = numeric())

for (i in 1:length(C)) {
  filtrados <- filter(LIC, LIC$REGBIO == C[i])
  for (j in 1){
    rand_LIC <- filtrados#[sample(nrow(filtrados), size = 190),]
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
    SER_LIC_2$ELEVATION <- median(na.omit(rand_LIC$ELEV))
    SER_LIC_2$SLOPE <- median(na.omit(rand_LIC$SLO))
    SER_LIC_2$TCD <- median(na.omit(rand_LIC$TCD))
    SER_LIC_2$HFI <- median(na.omit(rand_LIC$HFI))
    
    SER_LIC_2 <-  cbind(SER_LIC_2, SER_1)
    SER_LIC_REGBIO <-  rbind(SER_LIC_REGBIO, SER_LIC_2)
  }
  
}



## NP ----

SER_NP_REGBIO <-  data.frame(
  REGBIO = character(),
  SER = numeric(),
  ELEVATION = numeric(),
  SLOPE = numeric(),
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
    SER_NP_2$ELEVATION <- median(na.omit(rand_NP$ELE))
    SER_NP_2$SLOPE <- median(na.omit(rand_NP$SLO))
    SER_NP_2$TCD <- median(na.omit(rand_NP$TCD))
    SER_NP_2$HFI <- median(na.omit(rand_NP$HFI))
    
    SER_NP_2 <-  cbind(SER_NP_2, SER_1)
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

SER_LIC_REGBIO <- SER_LIC_REGBIO[, -c(7,8)]
SER_NP_REGBIO <- SER_NP_REGBIO[, -c(7,8)]

# SER promedio LIC por REGBIO
a <- SER_LIC_REGBIO %>%
  dplyr::group_by(REGBIO = fct_inorder(REGBIO)) %>%
  summarise(SER = median(SER),
            ELEVATION = median(ELEVATION), 
            SLOPE = median(SLOPE),
            TCD = median(TCD),
            HFI = median(HFI))
# SER promedio NP por REGBIO
b <- SER_NP_REGBIO %>%
  group_by(REGBIO = fct_inorder(REGBIO)) %>%
  summarise(SER = median(SER),
            ELEVATION = median(ELEVATION), 
            SLOPE = median(SLOPE),
            TCD = median(TCD),
            HFI = median(HFI))

# Tablas resultados
RESULT_REGBIO <- as.data.frame(cbind(a[,1],a[,2], b[,2], a[,3], b[,3],a[,4], b[,4],a[,5], b[,5], a[,6], b[,6]))


colnames(RESULT_REGBIO) <- c("REGBIO", 
                             "SER_LIC", "SER_NP",
                             "ELEVATION_LIC", "ELEVATION_NP",
                             "SLOPE_LIC", "SLOPE_NP",
                             "TDC_LIC", "TDC_NP",
                             "HFI_LIC", "HFI_NP")


writexl::write_xlsx(RESULT_REGBIO, "B:/A_GABRIEL/A_LIC_2dRUE/6_RESULTS/RESULT_REGBIO.xlsx")

kk1 <- SER_LIC_REGBIO[, c(1, 2)]
kk1 <- melt(kk1)

kk2 <- SER_NP_REGBIO[, c(1, 2)]
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