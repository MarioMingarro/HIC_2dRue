closeAllConnections()
rm(list=(ls()[ls()!="v"]))
gc(reset=TRUE)
source("Functions.R")


# Density plot 2dRUE class --------
## CCAA----
LIC_CCAA <- SER_LIC_CCAA_ALL[,c(1, 7:16)]
NP_CCAA <- SER_NP_CCAA_ALL[,c(1, 6:15)]
LIC_CCAA <- reshape2::melt(LIC_CCAA)
NP_CCAA <- reshape2::melt(NP_CCAA)

ggplot()+
  geom_point(data= LIC_CCAA, aes(x= variable, y = value, group = CCAA), col = "aquamarine3", size = 2, alpha =.5)+
  geom_line(data= LIC_CCAA, aes(x= variable, y = value, group = CCAA), col = "aquamarine3", alpha =.5)+
  geom_point(data= NP_CCAA, aes(x= variable, y = value, group = CCAA), col = "coral3", size = 2, alpha =.5)+
  geom_line(data= NP_CCAA, aes(x= variable, y = value, group = CCAA), col = "coral3", alpha =.5)+
  facet_wrap(~CCAA, ncol=3)+
  geom_vline(xintercept = 5, col = "red", alpha = .5, t)+
  labs(y = "%")+
  theme(strip.background = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        legend.position = "none",
        panel.grid.major.y = element_line(size = 0.1, linetype = 'dashed',
                                          colour = alpha("gray60",0.5)),
        panel.grid.major.x = element_line(size = 0.1, linetype = 'solid',
                                          colour = "gray60"),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = "mono", angle = 45, size = 10, hjust = 1),
        axis.text.y = element_text(family = "mono", size = 10),
        axis.title = element_text(family = "mono", angle = 90, size = 12),
        strip.text = element_text(family = "mono", size = 10))

## REGBIO----
LIC_REGBIO <- SER_LIC_REGBIO[,c(1, 8:17)]
LIC_REGBIO[,1] <- c("Atlantic", "Alpine", "Mediterranean")
NP_REGBIO <- SER_NP_REGBIO[,c(1, 8:17)]
NP_REGBIO[,1] <- c("Atlantic", "Alpine", "Mediterranean")
LIC_REGBIO <- reshape2::melt(LIC_REGBIO)
NP_REGBIO <- reshape2::melt(NP_REGBIO)

ggplot()+
  geom_point(data= LIC_REGBIO, aes(x= variable, y = value, group = REGBIO), col = "aquamarine3", size = 2, alpha =.5)+
  geom_line(data= LIC_REGBIO, aes(x= variable, y = value, group = REGBIO), col = "aquamarine3", alpha =.5)+
  geom_point(data= NP_REGBIO, aes(x= variable, y = value, group = REGBIO), col = "coral3", size = 2, alpha =.5)+
  geom_line(data= NP_REGBIO, aes(x= variable, y = value, group = REGBIO), col = "coral3", alpha =.5)+
  facet_wrap(~REGBIO, nrow = 3)+
  geom_vline(xintercept = 5, col = "red", alpha = .5, t)+
  labs(y = "%")+
  theme(strip.background = element_blank(),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        legend.position = "none",
        panel.grid.major.y = element_line(size = 0.1, linetype = 'dashed',
                                          colour = "gray60"),
        panel.grid.major.x = element_line(size = 0.1, linetype = 'solid',
                                          colour = "gray60"),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(family = "mono", angle = 45, size = 10, hjust = 1),
        axis.text.y = element_text(family = "mono", size = 10),
        axis.title = element_text(family = "mono", angle = 90, size = 12),
        strip.text = element_text(family = "mono", size = 10))

# Distancias ----
## CCAA----
library(emdist)
distancias <- data.frame(
  CCAA = character(),
  DISTANCES = numeric())

for(i in 1:length(C)){
  kk <- data.frame(
    CCAA = "a",
    DISTANCES = 1)
  a <- filter(LIC_CCAA, LIC_ == C[i])
  b <- filter(NP_CCAA, NP_CCAA$CCAA == C[i])
  a <- as.matrix(a[,c(2:11)])
  b <- as.matrix(b[,c(2:11)])
  kk$CCAA <- C[i]
  kk$DISTANCES <- emd2d(a, b, xdist = 1, ydist = 1, dist="euclidean")
  distancias <- rbind(distancias, kk)
}



## REGBIO----
LIC_REGBIO <- SER_LIC_REGBIO[,c(1, 8:17)]
NP_REGBIO <- SER_NP_REGBIO[,c(1, 8:17)]


distancias <- data.frame(
  REGBIO = character(),
  DISTANCES = numeric())

for(i in 1:length(C)){
  kk <- data.frame(
    REGBIO = "a",
    DISTANCES = 1)
  a <- filter(LIC_REGBIO, LIC_REGBIO$REGBIO == C[i])
  b <- filter(NP_REGBIO, NP_REGBIO$REGBIO == C[i])
  a <- as.matrix(a[,c(2:11)])
  b <- as.matrix(b[,c(2:11)])
  kk$REGBIO <- C[i]
  kk$DISTANCES <- emd2d(a, b, xdist = 1, ydist = 1, dist="euclidean")
  distancias <- rbind(distancias, kk)
}


# Asimetria -----
## CCAA----
library(psych)

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

## REGBIO----
asimetria_REGBIO <- data.frame(
  CCAA = character(),
  LIC = numeric(),
  NP = numeric())

for(i in 1:length(C)){
  kk <- data.frame(
    CCAA = "a",
    LIC = 1,
    NP = 1)
  a <- filter(LIC, LIC$REGBIO == C[i])
  b <- filter(NP, NP$REGBIO == C[i])
  aa <- a$gridcode
  bb <- b$gridcode
  kk$CCAA <- C[i]
  kk$LIC <- skew(c(aa))
  kk$NP <- skew(c(bb))
  asimetria_REGBIO <- rbind(asimetria_REGBIO, kk)
}

