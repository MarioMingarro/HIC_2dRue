library(tidyverse)
library(stringr)
library(ggstatsplot)
library(extrafont)



Data <- read.csv2("D:/GABRIEL/RESULTADOS/THIC_RN_2dRUE_AN_2.txt", encoding="UTF-8")

Data$S_NAT3_c_1 <- str_replace(Data$S_NAT3_c_1 , ".*_", "")

Data$FIGURA <- replace(Data$FIGURA, Data$FIGURA == "LIC", "RN2000")
Data$FIGURA <- replace(Data$FIGURA, Data$FIGURA == "ZEPA", "RN2000")# THIC 44
# Figura 50
# RUE 54
# Area 55

Data <- Data[,c(44,50,54,55)]
colnames(Data) <- c("THIC", "FIGURA", "RUE", "AREA_m")
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


rm(sel)
aaa <- unique(Data$THIC)
RUE <- unique(Data$RUE)

Tabla <- data.frame(THIC= character(),
                    RUE = character(),
                    p = numeric())

for (i in 2:length(aaa)){
  sel <- dplyr::filter(Data, Data$THIC == aaa[i])
  Tabla_1 <- data.frame(THIC = character(),
                      RUE = character(),
                      p = numeric())
  
  for (j in 1:length(RUE)){
    aa <- dplyr::filter(sel, sel$RUE == RUE[j])
    try({
    WT <- wilcox.test(AREA_km ~ FIGURA, data = aa,
                exact = FALSE)
    })
    
    print(paste0(aaa[i], "_", RUE[j]))
    
    Tabla_2 <- data.frame(THIC = "A",
                          RUE = "A",
                          p = 2)
    Tabla_2[1,1] <- THIC[i]
    Tabla_2[1,2] <- RUE[j]
    Tabla_2[1,3] <- WT$p.value
    Tabla_1 <- rbind(Tabla_1, Tabla_2)
    
    

  }
  Tabla <- rbind(Tabla, Tabla_1)
}
Tabla$p <-round(Tabla$p,4)

writexl::write_xlsx(Tabla, "D:/GABRIEL/RESULTADOS/NEW/wilcoxon_THIC_RUE.xlsx")

Areas <- Data %>% 
  group_by(THIC, FIGURA, RUE) %>% 
  summarise(media = mean(AREA_km)) %>% 
  pivot_wider(names_from = THIC, values_from = media)

writexl::write_xlsx(Areas, "D:/GABRIEL/RESULTADOS/NEW/Areas_THIC_RUE.xlsx")


library(formattable)
Tabla_kk <- formattable(Tabla, list(
  p = formatter("span", 
                        style = ~ style(color = ifelse(Tabla$p < 0.05, "black", "red")))))


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
