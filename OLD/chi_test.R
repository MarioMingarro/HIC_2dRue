library(tidyverse)
library(stringr)

Data1 <- read.csv2("E:/GABRIEL/RESULTADOS/9340_RN2000_P_2dRUE.txt", encoding="UTF-8")
Data2 <- read.csv2("E:/GABRIEL/RESULTADOS/9340_N_RN2000_P_2dRUE.txt", encoding="UTF-8")

Data1 <- Data1[, 3:6]

Data2 <- Data2[, 3:6]


colnames(Data1) <- c("X", "Y", "RUE", "FIGURA")
colnames(Data2) <- c("X", "Y", "RUE", "FIGURA")

Data1 <- Data1[Data1$RUE >= 0, ]
Data2 <- Data2[Data2$RUE >= 0, ]

Data <- rbind(Data1, Data2)

hist(Data1$RUE)
hist(Data2$RUE)
ggplot(Data, aes(x=RUE, col = FIGURA)) + 
  geom_density()


kk <- Data %>% 
  group_by(FIGURA) %>% 
  count(RUE)

N_RN2000 <- kk[1:10,2:3]
RN_2000 <- kk[10:20,2:3]
colnames(N_RN2000) <- c("RUE", "No Protegido")
colnames(RN_2000) <- c("RUE", "RN2000")
dt <- left_join(RN_2000, N_RN2000)
dt <- dt[,-1]


chisq.test(dt)


