library(tidyverse)
library(stringr)


Data <- read.csv2("E:/GABRIEL/RESULTADOS/NEW/THIC_RN_2dRUE_AN_2.txt", encoding="UTF-8")


Data <- Data[,c(44,50,54,55)]
colnames(Data) <- c("THIC", "FIGURA", "RUE", "AREA_m")
Data$AREA_km <- cbind(Data$AREA/1000000)
9320
9340
aa <- dplyr::filter(Data, THIC == "9340")

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
chisq$p.value
chisq$residuals
library(corrplot)
corrplot(chisq$residuals, is.cor = FALSE)

contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)

corrplot(contrib, is.cor = FALSE)


library(RColorBrewer)
counts <- table(kk)
barplot(counts, 
        col=brewer.pal(n = 2, name = "Set2"), 
        legend = rownames(counts), las = 2)
