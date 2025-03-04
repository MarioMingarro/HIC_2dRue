closeAllConnections()
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Functions.R")

# CCAA ----
## LIC 
# Crear un nuevo dataframe donde se almacenarán los resultados
C <- unique(LIC$CCAA)

SER_LIC_CCAA <-  data.frame(
  CCAA = character(),
  SER = numeric(),
  ELEVATION = numeric(),
  SLOPE = numeric(),
  TCD = numeric(),
  HFI = numeric())


for (i in 1:length(C)) {
  filtrados <- filter(LIC, LIC$CCAA == C[i])
  for (j in 1:1000){
    rand_LIC <- filtrados[sample(nrow(filtrados), size = 190),]
    SER_1 <- data.frame(
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
      SLOPE = 2,
      TCD = 2,
      HFI = 2
    )
    
    SER_LIC_2$CCAA <- C[i]
    SER_LIC_2$SER <- SER_2
    SER_LIC_2$ELEVATION <- median(na.omit(rand_LIC$ELEV))
    SER_LIC_2$SLOPE <- median(na.omit(rand_LIC$SLO))
    SER_LIC_2$TCD <- median(na.omit(rand_LIC$TCD))
    SER_LIC_2$HFI <- median(na.omit(rand_LIC$HFI))
    
    SER_LIC_2 <-  cbind( SER_LIC_2, SER_1)
    SER_LIC_CCAA <-  rbind(SER_LIC_CCAA, SER_LIC_2)
    
  }
}

## NP 
# Crear un nuevo dataframe donde se almacenarán los resultados

SER_NP_CCAA <-  data.frame(
  CCAA = character(),
  SER = numeric(),
  ELEVATION = numeric(),
  SLOPE = numeric(),
  TCD = numeric(),
  HFI = numeric())

for (i in 1:length(C)) {
  filtrados <- filter(NP, NP$CCAA == C[i])
  for (j in 1:1000){
    rand_NP <- filtrados[sample(nrow(filtrados), size = 190),]
    SER_1 <- data.frame(
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
      SLOPE = 2,
      TCD = 2,
      HFI = 2)
    
    SER_NP_2$CCAA <- C[i]
    SER_NP_2$SER <- SER_2
    SER_NP_2$ELEVATION <- median(na.omit(rand_NP$ELE))
    SER_NP_2$SLOPE <- median(na.omit(rand_NP$SLO))
    SER_NP_2$TCD <- median(na.omit(rand_NP$TCD))
    SER_NP_2$HFI <- median(na.omit(rand_NP$HFI))
    SER_NP_2 <-  cbind(SER_NP_2, SER_1)
    SER_NP_CCAA <-  rbind(SER_NP_CCAA, SER_NP_2)
  }
  
}

SER_LIC_CCAA <- SER_LIC_CCAA[, -c(7,8)]
SER_NP_CCAA <- SER_NP_CCAA[, -c(7,8)]
rm(filtrados, SER_1, SER_LIC_2, SER_NP_2, rand_LIC, rand_NP)


SER_LIC_CCAA <- mutate(SER_LIC_CCAA, CCAA_C =
                         case_when(
                           SER_LIC_CCAA$CCAA == "Galicia"                     ~ "GA", 
                           SER_LIC_CCAA$CCAA == "Asturias, Principado de"     ~ "AS",
                           SER_LIC_CCAA$CCAA == "Cantabria"                   ~ "CN",
                           SER_LIC_CCAA$CCAA == "País Vasco"                  ~ "PV",
                           SER_LIC_CCAA$CCAA == "Comunidad Foral de Navarra"  ~ "NA",
                           SER_LIC_CCAA$CCAA == "Castilla y León"             ~ "CL",
                           SER_LIC_CCAA$CCAA == "Aragón"                      ~ "AR",
                           SER_LIC_CCAA$CCAA == "Cataluña"                    ~ "CT",
                           SER_LIC_CCAA$CCAA == "Rioja, La"                   ~ "LR",
                           SER_LIC_CCAA$CCAA == "Castilla - La Mancha"        ~ "CM",
                           SER_LIC_CCAA$CCAA == "Comunidad de Madrid"         ~ "MA",
                           SER_LIC_CCAA$CCAA == "Comunitat Valenciana"        ~ "CV",
                           SER_LIC_CCAA$CCAA == "Extremadura"                 ~ "EX",
                           SER_LIC_CCAA$CCAA == "Andalucía"                   ~ "AN",
                           SER_LIC_CCAA$CCAA == "Región de Murcia"            ~ "MU"))
SER_NP_CCAA <- mutate(SER_NP_CCAA, CCAA_C =
                        case_when(
                          SER_LIC_CCAA$CCAA == "Galicia"                     ~ "GA", 
                          SER_LIC_CCAA$CCAA == "Asturias, Principado de"     ~ "AS",
                          SER_LIC_CCAA$CCAA == "Cantabria"                   ~ "CN",
                          SER_LIC_CCAA$CCAA == "País Vasco"                  ~ "PV",
                          SER_LIC_CCAA$CCAA == "Comunidad Foral de Navarra"  ~ "NA",
                          SER_LIC_CCAA$CCAA == "Castilla y León"             ~ "CL",
                          SER_LIC_CCAA$CCAA == "Aragón"                      ~ "AR",
                          SER_LIC_CCAA$CCAA == "Cataluña"                    ~ "CT",
                          SER_LIC_CCAA$CCAA == "Rioja, La"                   ~ "LR",
                          SER_LIC_CCAA$CCAA == "Castilla - La Mancha"        ~ "CM",
                          SER_LIC_CCAA$CCAA == "Comunidad de Madrid"         ~ "MA",
                          SER_LIC_CCAA$CCAA == "Comunitat Valenciana"        ~ "CV",
                          SER_LIC_CCAA$CCAA == "Extremadura"                 ~ "EX",
                          SER_LIC_CCAA$CCAA == "Andalucía"                   ~ "AN",
                          SER_LIC_CCAA$CCAA == "Región de Murcia"            ~ "MU"))

## SER ACTUALIZADO (2025) ----
# Crear un nuevo dataframe donde se almacenarán los resultados
C <- unique(NP$CCAA)

SER_NP_CCAA <- data.frame(
  CCAA = character(),
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
  AAR = numeric()
)

# Nombres de columnas de SER asociadas a los códigos de gridcode
columnas_SER <- c("ABR", "BAS", "MDEG", "DEG", "PBB", "PAB", "SMAD", "MAD", "REF", "AAR")
names(columnas_SER) <- 1:10 # Suponiendo gridcodes del 1 al 10

for (i in 1:length(C)) {
  filtrados <- dplyr::filter(NP, NP$CCAA == C[i])
  for (j in 1:1000) {
    rand_NP <- filtrados[sample(nrow(filtrados), size = 190), ]
    
    # Calcular el porcentaje de cada valor en la columna 'gridcode'
    porcentaje_por_valor <- prop.table(table(rand_NP$gridcode)) * 100
    
    # Crear un dataframe SER inicializado en 0
    SER <- as.data.frame(matrix(0, nrow = 1, ncol = length(columnas_SER)))
    colnames(SER) <- columnas_SER
    
    # Asignar valores según gridcode
    for (gc in names(porcentaje_por_valor)) {
      if (gc %in% names(columnas_SER)) {
        columna <- columnas_SER[[gc]]
        SER[[columna]] <- porcentaje_por_valor[[gc]]
      }
    }
    
    # Calcular SR_b y SR_a asegurando que los valores fuera de rango sean 0
    SR_b <- sum(SER[, c("PAB", "SMAD", "MAD", "REF", "AAR")], na.rm = TRUE)
    SR_a <- sum(SER[, c("MDEG", "DEG", "PBB")], na.rm = TRUE)
    
    # Crear dataframe de resultados
    SER_NP_2 <- data.frame(
      CCAA = C[i],
      SER = ifelse((SR_b + SR_a) == 0, 0, (SR_b - SR_a) / (SR_b + SR_a)))
    
    # Unir SER con SER_NP_2
    SER_NP_2 <- cbind(SER_NP_2, SER)
    
    # Agregar a SER_NP_CCAA
    SER_NP_CCAA <- rbind(SER_NP_CCAA, SER_NP_2)
  }
}

SER_NP_CCAA %>% group_by(CCAA) %>% summarise(median(SER))

## COMPARACION SER CCAA ----


# SER promedio LIC por CCAA
a <- SER_LIC_CCAA %>%
  group_by(CCAA = fct_inorder(CCAA)) %>%
  summarise(SER = median(SER),
            ELEVATION = median(ELEVATION), 
            SLOPE = median(SLOPE),
            TCD = median(TCD),
            HFI = median(HFI))

# SER promedio NP por CCAA
b <- SER_NP_CCAA %>%
  group_by(CCAA = fct_inorder(CCAA)) %>%
  summarise(SER = median(SER),
            ELEVATION = median(ELEVATION), 
            SLOPE = median(SLOPE),
            TCD = median(TCD),
            HFI = median(HFI))

# Tablas resultados

RESULT_CCAA <- as.data.frame(cbind(a[,1],a[,2], b[,2], a[,3], b[,3],a[,4], b[,4],a[,5], b[,5], a[,6], b[,6]))

colnames(RESULT_CCAA) <- c("CCAA", 
                           "SER_LIC", "SER_NP",
                           "ELEVATION_LIC", "ELEVATION_NP",
                           "SLOPE_LIC", "SLOPE_NP",
                           "TDC_LIC", "TDC_NP",
                           "HFI_LIC", "HFI_NP")


writexl::write_xlsx(RESULT_CCAA, "B:/A_GABRIEL/A_LIC_2dRUE/6_RESULTS/RESULT_CCAA.xlsx")

kk1 <- SER_LIC_CCAA[, c(15, 2)]
kk1 <- melt(kk1)

kk2 <- SER_NP_CCAA[, c(15, 2)]
kk2 <- melt(kk2)

# Gráfico resultados

## Figure S10
ggplot()+
  geom_boxplot(data= kk1, aes(y = value, x = factor(CCAA_C)), 
               fill = "aquamarine3", 
               colour ="aquamarine3", 
               alpha =.5,
               position = position_nudge(x=-.1),
               width = 0.25)+
  stat_boxplot(data= kk1, aes(y = value, x = factor(CCAA_C)), 
               colour = "aquamarine3",
               alpha =.5,
               position = position_nudge(x=-.1),
               geom="errorbar", width=0.5)+
  geom_boxplot(data= kk2, aes(y = value, x = factor(CCAA_C)), 
               fill = "coral3",
               colour = "coral3",
               alpha =.5,
               position = position_nudge(x=.1),
               width = 0.25)+
  stat_boxplot(data= kk2, aes(y = value, x = factor(CCAA_C)),
               colour = "coral3",
               alpha =.5,
               position = position_nudge(x=.1),
               geom="errorbar", width=0.5)+
  ylim(-1,1)+
  labs(y = "SER")+
  theme(strip.background = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        legend.position = "none",
        panel.grid.major.y = element_line(size = 0.1, linetype = 'dashed',
                                          colour = "gray60"),
        panel.grid.major.x = element_line(size = 0.1, linetype = 'dashed',
                                          colour = "gray60"),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = "mono", angle = 45, size = 12, hjust = 1, colour = "gray10"),
        axis.text.y = element_text(family = "mono", size = 10,colour = "gray10"),
        axis.title = element_text(family = "mono", angle = 90, size = 12),
        strip.text = element_text(family = "mono", size = 10))
  
# Test de wilcoxon 
SER_CCAA_COMPARATION_FINAL <-  data.frame(
  CCAA = character(),
  Anderson_Darling_LIC = numeric(),
  Anderson_Darling_NP = numeric(),
  wilcox.test.pvalue = numeric(),
  wilcox.test.W = numeric())

for( i in 1:length(C)){
  SER_CCAA_COMPARATION <-  data.frame(
    CCAA = "a",
    Anderson_Darling_LIC = 1,
    Anderson_Darling_NP = 1,
    wilcox.test.pvalue = 1,
    wilcox.test.W = 1)
  
  a <- filter(SER_LIC_CCAA,  CCAA== paste0(C[i]))
  b <- filter(SER_NP_CCAA,  CCAA == paste0(C[i]))
  # "SER" "ELEVATION" "SLOPE" "TCD" "HFI" 
  a <- a$HFI
  b <- b$HFI
  
  SER_CCAA<- as.data.frame(cbind(a, b))
  colnames(SER_CCAA) <- c("LIC", "NP")
  
  SER_CCAA_COMPARATION$CCAA <- C[i]
  SER_CCAA_COMPARATION$Anderson_Darling_LIC <- ad.test(SER_CCAA$LIC)$p.value
  #SER_CCAA_COMPARATION$Anderson_Darling_NP <- ad.test(SER_CCAA$NP)$p.value
  SER_CCAA_COMPARATION$wilcox.test.pvalue <- wilcox.test(SER_CCAA$LIC, SER_CCAA$NP, paired = T, conf.level = 0.95)$p.value
  SER_CCAA_COMPARATION$wilcox.test.W <- wilcox.test(SER_CCAA$LIC, SER_CCAA$NP, paired = T, conf.level = 0.99)$statistic
  SER_CCAA_COMPARATION_FINAL <- rbind(SER_CCAA_COMPARATION_FINAL, SER_CCAA_COMPARATION)
}

wilcox.test(a , b, paired = T)
mean(a$SER)
mean(b$SER)

# Correlaciones
cor_LIC <- SER_LIC_CCAA[,2:6]
cor_LIC <- cor(cor_LIC)
cor_NP <- SER_NP_CCAA[,2:6]
cor_NP <- cor(cor_NP)
corrplot(cor_LIC, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
corrplot(cor_NP, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))




# REGBIO ----
## LIC 
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
  for (j in 1:1000){
    rand_LIC <- filtrados[sample(nrow(filtrados), size = 190),]
    SER_1 <- data.frame(
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
      SLOPE = 2,
      TCD = 2,
      HFI = 2
    )
    
    SER_LIC_2$REGBIO <- C[i]
    SER_LIC_2$SER <- SER_2
    SER_LIC_2$ELEVATION <- median(na.omit(rand_LIC$ELEV))
    SER_LIC_2$SLOPE <- median(na.omit(rand_LIC$SLO))
    SER_LIC_2$TCD <- median(na.omit(rand_LIC$TCD))
    SER_LIC_2$HFI <- median(na.omit(rand_LIC$HFI))
    
    SER_LIC_2 <-  cbind( SER_LIC_2, SER_1)
    SER_LIC_REGBIO <-  rbind(SER_LIC_REGBIO, SER_LIC_2)
    
  }
}

## NP 
# Crear un nuevo dataframe donde se almacenarán los resultados
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
      SLOPE = 2,
      TCD = 2,
      HFI = 2)
    
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
rm(filtrados, SER_1, SER_LIC_2, SER_NP_2, rand_LIC, rand_NP)


## COMPARACION SER REGBIO ----
# SER promedio LIC por REGBIO
a <- SER_LIC_REGBIO %>%
  group_by(REGBIO = fct_inorder(REGBIO)) %>%
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


#writexl::write_xlsx(RESULT_REGBIO, "B:/A_GABRIEL/A_LIC_2dRUE/6_RESULTS/RESULT_REGBIO.xlsx")

kk1 <- SER_LIC_REGBIO[, c(1, 2)]
kk1 <- melt(kk1)

kk2 <- SER_NP_REGBIO[, c(1, 2)]
kk2 <- melt(kk2)

# Gráfico resultados

ggplot()+
  geom_boxplot(data= kk1, aes(y = value, x = factor(REGBIO)), 
               fill = "aquamarine3", 
               colour ="aquamarine3", 
               alpha =.5,
               position = position_nudge(x=-.1),
               width = 0.25)+
  stat_boxplot(data= kk1, aes(y = value, x = factor(REGBIO)), 
               colour = "aquamarine3",
               alpha =.5,
               position = position_nudge(x=-.1),
               geom="errorbar", width=0.5)+
  geom_boxplot(data= kk2, aes(y = value, x = factor(REGBIO)), 
               fill = "coral3",
               colour = "coral3",
               alpha =.5,
               position = position_nudge(x=.1),
               width = 0.25)+
  stat_boxplot(data= kk2, aes(y = value, x = factor(REGBIO)),
               colour = "coral3",
               alpha =.5,
               position = position_nudge(x=.1),
               geom="errorbar", width=0.5)+
  ylim(-1,1)+
  labs(y = "SER")+
  theme(strip.background = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        legend.position = "none",
        panel.grid.major.y = element_line(size = 0.1, linetype = 'dashed',
                                          colour = "gray60"),
        panel.grid.major.x = element_line(size = 0.1, linetype = 'dashed',
                                          colour = "gray60"),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = "mono", angle = 45, size = 12, hjust = 1, colour = "gray10"),
        axis.text.y = element_text(family = "mono", size = 10,colour = "gray10"),
        axis.title = element_text(family = "mono", angle = 90, size = 12),
        strip.text = element_text(family = "mono", size = 10))

# Test de wilcoxon 
SER_REGBIO_COMPARATION_FINAL <-  data.frame(
  REGBIO = character(),
  Anderson_Darling_LIC = numeric(),
  Anderson_Darling_NP = numeric(),
  wilcox.test.pvalue = numeric(),
  wilcox.test.W = numeric())

for( i in 1:length(C)){
  SER_REGBIO_COMPARATION <-  data.frame(
    REGBIO = "a",
    Anderson_Darling_LIC = 1,
    Anderson_Darling_NP = 1,
    wilcox.test.pvalue = 1,
    wilcox.test.W = 1)
  
  a <- filter(SER_LIC_REGBIO,  REGBIO== paste0(C[i]))
  b <- filter(SER_NP_REGBIO,  REGBIO == paste0(C[i]))
  # "SER" "ELEVATION" "SLOPE" "TCD" "HFI" 
  a <- a$HFI
  b <- b$HFI
  
  SER_REGBIO<- as.data.frame(cbind(a, b))
  colnames(SER_REGBIO) <- c("LIC", "NP")
  
  SER_REGBIO_COMPARATION$REGBIO <- C[i]
  SER_REGBIO_COMPARATION$Anderson_Darling_LIC <- ad.test(SER_REGBIO$LIC)$p.value
  #SER_REGBIO_COMPARATION$Anderson_Darling_NP <- ad.test(SER_REGBIO$NP)$p.value
  SER_REGBIO_COMPARATION$wilcox.test.pvalue <- wilcox.test(SER_REGBIO$LIC, SER_REGBIO$NP, paired = T)$p.value
  SER_REGBIO_COMPARATION$wilcox.test.W <- wilcox.test(SER_REGBIO$LIC, SER_REGBIO$NP, paired = T)$statistic
  SER_REGBIO_COMPARATION_FINAL <- rbind(SER_REGBIO_COMPARATION_FINAL, SER_REGBIO_COMPARATION)
}

wilcox.test(a$SER, b$SER, paired = T)
mean(a$SER)
mean(b$SER)

# Correlaciones
cor_LIC <- SER_LIC_REGBIO[,2:6]
cor_LIC <- cor(cor_LIC)
cor_NP <- SER_NP_REGBIO[,2:6]
cor_NP <- cor(cor_NP)
corrplot(cor_LIC, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
corrplot(cor_NP, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))


qqnorm(SER_LIC_CCAA$SER)
hist(SER_NP_CCAA$SER)

library(lmodel2)
lmodel2(SER~SLOPE, data = SER_LIC_CCAA)
lmodel2(SER~TCD, data = SER_LIC_CCAA)
lmodel2(SER~HFI, data = SER_LIC_CCAA)
lm1 <- lm(SER~SLOPE*CCAA+TCD*CCAA+HFI*CCAA, data = SER_LIC_CCAA)
qqnorm(residuals(lm1))
check_model(lm1)

ad.test(SER_LIC_CCAA$SER)
norm <- rnorm(100, mean = 5, sd = 3)
ad.test(norm)

library(car)
relaciones <- ~SER+ELEVATION+SLOPE+TCD+HFI
pairs(relaciones, data=SER_LIC_CCAA)
## pch=".", pch="0", pch="1"; para exactamente eso; 
## pch=0 (cuadrado vacio), pch=1 (circulo vacio), pch=2 (triangulo vacio), pch=5 (rombo vacio)
## panel con valores de correlaciones
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="complete.obs"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, col="blue", cex = cex.cor * (1 + r) / 0.1)    ## cambiar el tamannio de la letra modificando "0.25" en el denominador de "cex = cex.cor * (1 + r) / 0.25"
}
scatterplotMatrix(relaciones, data=SER_LIC_CCAA, pch=19, cex=0.2, cex.lab=3, cex.axis=1.5, diagonal=TRUE, col="red", 
                  col.axis='black', regLine=list(method=lm, col="blue", lwd=2), smooth=TRUE,
                  main="los numeros son r lineales en valor absoluto", lower.panel=panel.cor)
