library(tidyverse)
library(stringr)
library(pwrss)

Data <- read.csv2("D:/GABRIEL/RESULTADOS/NEW/THIC_RN_2dRUE_AN_2.txt", encoding="UTF-8")


Data <- Data[,c(44,50,54,55)]
colnames(Data) <- c("THIC", "FIGURA", "RUE", "AREA_m")
Data$AREA_km <- cbind(Data$AREA/1000000)
9320
9340
aa <- dplyr::filter(Data, THIC == "9320")

kk <- aa %>% 
  group_by(RUE, FIGURA) %>% 
  summarise(media = sum(AREA_m)) %>% 
  pivot_wider(names_from = RUE, values_from = media)

kk <- t(kk)
colnames(kk) <- kk[1,]
kk <- kk[-1,]
kk <- as.data.frame(kk)
kk$`No protegido` <- as.numeric(kk$`No protegido`)
kk$RN2000 <- as.numeric(kk$RN2000 )

chisq <- chisq.test(kk)
chisq2 <- chisq.test(kk, simulate.p.value = TRUE)

chisq$observed
chisq$expected
library(corrplot)
corrplot(chisq$residuals, is.cor = FALSE)
