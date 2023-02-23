library(tidyverse)
library(stringr)
library(ggstatsplot)
library(extrafont)



Data <- read.csv2("A:/GABRIEL/RESULTADOS/RESULTADOS_AN.txt", encoding="UTF-8")

Data$S_NAT3_c_1 <- str_replace(Data$S_NAT3_c_1 , ".*_", "")

# "Texto" 4 PROVINCIA
# "FIGURA" 13 FIGURA
# "NOMBRE" 14 Nombre PA
# "S_NAT3_c_1" 34 HIC
# "Estado"  28 RUE
# "AREA" 37 area

Data <- Data[,c(4,13,14,34,28,40)]
colnames(Data) <- c("PROVINCIA", "FIGURA", "NOMBRE", "HIC", "RUE", "AREA_m")
Data$FIGURA <- replace(Data$FIGURA, Data$FIGURA == "LIC", "RN2000")
Data$FIGURA <- replace(Data$FIGURA, Data$FIGURA == "ZEPA", "RN2000")
Data$AREA_km <- cbind(Data$AREA/1000000)

Data$RUE_CODE <- Data$RUE
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "Degradado", "1")
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "Productivo de baja biomasa", "2")
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "Basal"  , "3")
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "Submaduro", "4")
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "Maduro", "5")
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "Productivo de alta biomasa", "6")
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "De referencia", "7")
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "Muy degradado", "8")
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "Anomaias de bajo rendimiento", "9")
Data$RUE_CODE <- replace(Data$RUE_CODE, Data$RUE_CODE == "Anomalias de alto rendimiento", "10")



HIC <- unique(Data$HIC)
RUE <- unique(Data$RUE)

Tabla <- data.frame(HIC= character(),
                    RUE = character(),
                    p = numeric())

for (i in 1:length(HIC)){
  sel <- dplyr::filter(Data, Data$HIC == HIC[i])
  cat(paste0("\n\n## ", HIC[i], "\n"))
  Tabla_1 <- data.frame(HIC= character(),
                      RUE = character(),
                      p = numeric())
  for (j in 1:length(RUE)){
    aa <- dplyr::filter(sel, sel$RUE == RUE[j])
    RN2000 <- dplyr::filter(aa, aa$FIGURA == "RN2000")
    NP <- dplyr::filter(aa, aa$FIGURA == "RN2000")
    WT <- wilcox.test(AREA_km ~ FIGURA, data = aa,
                exact = FALSE)
    
    
    Tabla_2 <- data.frame(HIC = "A",
                          RUE = "A",
                          p = 2)
    Tabla_2[1,1] <- HIC[i]
    Tabla_2[1,2] <- RUE[j]
    Tabla_2[1,2] <- WT$p.value
    
    print(ggbetweenstats(aa, 
                         FIGURA, AREA_km, 
                         bf.message = FALSE,
                         plot.type = "boxviolin", 
                         type = "nonparametric",
                         xlab = "Figura",
                         ylab = bquote("Area " (km^2)),
                         title = paste0(RUE[j]),
                         outlier.color = "grey50",
                         point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), alpha =
                                             0.2, size = 3, stroke = 0, na.rm = TRUE),
                         ggtheme = ggplot2::theme_gray(), 
                         package = "yarrr", 
                         palette = "bugs")+
            ggplot2::theme(legend.position = "none",
                           panel.background = element_rect(fill = "white", colour = "grey50"),
                           axis.title = element_text(size = 12, family="Comic Sans MS"))
    )
  }
}