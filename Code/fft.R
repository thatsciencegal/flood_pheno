library(lubridate)
library(chron)
library(tidyverse)
library(patchwork)

## Read in the data
dat.in <- "./Data/GEE_results/"
l8_for_up <- read.csv(paste0(dat.in,"l8_forest_upstream_sdm.csv"))
l8_sav_up <- read.csv(paste0(dat.in,"l8_savanna_upstream_sdm.csv"))
l8_for_sdm <- read.csv(paste0(dat.in,"l8_forest_sdm_cabr.csv"))
l8_sav_sdm <- read.csv(paste0(dat.in,"l8_savanna_sdm_cabr.csv"))
l5_for_up <- read.csv(paste0(dat.in,"l5_forest_upstream_sdm.csv"))
l5_sav_up <- read.csv(paste0(dat.in,"l5_savanna_upstream_sdm.csv"))
l5_for_sdm <- read.csv(paste0(dat.in,"l5_forest_sdm_cabr.csv"))
l5_sav_sdm <- read.csv(paste0(dat.in,"l5_savanna_sdm_cabr.csv"))

## Rename the data
new.head <- c("date","EVI","NDVI","NDWI","SAVI","gNDVI")

dfs <- c("l8_for_up","l8_sav_up","l8_for_sdm","l8_sav_sdm","l5_for_up","l5_sav_up",'l5_for_sdm','l5_sav_sdm')

for(df in dfs){
  assign(df, setNames(get(df),new.head))
}

## Put the dataframes in a list
## 1) Forest -> 2) Savanna upstream (landsat 8)
## 3) Forest -> 4) Savanna SDM (landsat 8)
## 5) Forest -> 6) Savanna upstream (landsat 5)
## 7) Forest -> 8) Savanna SSDM (landsat 5)
dfs <- list(l8_for_up,l8_sav_up,l8_for_sdm,l8_sav_sdm,l5_for_up,l5_sav_up,l5_for_sdm,l5_sav_sdm)

## Remove duplicate days
dfs <- lapply(dfs, function(x) x[!duplicated(x$date),])

## Change date to ymd format
func <- function(x){
  x$date <- mdy(x$date)
  return(x)
}

dfs <- lapply(dfs, func)

## Add in missing days
dat_l8<-seq.dates("07/04/2013","06/16/2018",16)
dat_l8<-data.frame(date=mdy(dat_l8))

for(i in 1:4){
 dfs[[i]]<-dfs[[i]] %>% right_join(dat_l8) %>% arrange(date)
}

dat_l5<-seq.dates("07/20/1984","06/30/2011",16)
dat_l5<-data.frame(date=mdy(dat_l5))

for(i in 5:8){
  dfs[[i]]<-dfs[[i]] %>% right_join(dat_l5) %>% arrange(date)
}

## Linearly interpolate missing values
f1 <- function(dat) {
  dat <- dat %>% mutate(filled=case_when(is.na(EVI)~"yes",
                                         !is.na(EVI)~"no"))
  
  evi <- approx(dat$date,dat$EVI,dat$date)
  ndvi <- approx(dat$date,dat$NDVI,dat$date)
  ndwi <- approx(dat$date,dat$NDWI,dat$date)
  savi <- approx(dat$date,dat$SAVI,dat$date)
  gndvi <- approx(dat$date,dat$gNDVI,dat$date)
  
  dat$EVI <- evi$y
  dat$NDVI <- ndvi$y
  dat$NDWI <- ndwi$y
  dat$SAVI <- savi$y
  dat$gNDVI <- gndvi$y
  
  dat <- pivot_longer(dat,cols=EVI:gNDVI,names_to="index",values_to="value")
  
  return(dat)
}

dfs <- lapply(dfs,f1)

p1<-ggplot(dfs[[1]],aes(x=date,y=value))+
  geom_line(aes(colour=index))+
  geom_point(aes(colour=index),size=1.5)+
  geom_point(aes(colour=filled),size=2)+
  ylim(c(-0.25,1))+
  labs(x="",y="Value")+
  ggtitle("(A) Forest VI upstream of SDM (2014-2018)")+
  scale_colour_manual(values=c("#a6cee3","#33a02c","#b2df8a","#1f78b4","#00000000","#a6611a","black"),
                      breaks=c("EVI","gNDVI","NDVI","NDWI","SAVI","Interpolated data"),
                      name="")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none")


p2<-ggplot(dfs[[2]],aes(x=date,y=value))+
  geom_line(aes(colour=index))+
  geom_point(aes(colour=index),size=1.5)+
  geom_point(aes(colour=filled),size=2)+
  ylim(c(-0.25,1))+
  labs(x="",y="")+
  ggtitle("(B) Savanna VI upstream of SDM (2014-2018)")+
  scale_colour_manual(values=c("#a6cee3","#33a02c","#b2df8a","#1f78b4","#00000000","#a6611a","black"),
                      breaks=c("EVI","gNDVI","NDVI","NDWI","SAVI","Interpolated data"),
                      name="")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

p3<-ggplot(dfs[[3]],aes(x=date,y=value))+
  geom_line(aes(colour=index))+
  geom_point(aes(colour=index),size=1.5)+
  geom_point(aes(colour=filled),size=2)+
  ylim(c(-0.25,1))+
  labs(x="Date",y="Value")+
  ggtitle("(C) Forest VI downstream of SDM (2014-2018)")+
  scale_colour_manual(values=c("#a6cee3","#33a02c","#b2df8a","#1f78b4","#00000000","#a6611a","black"),
                      breaks=c("EVI","gNDVI","NDVI","NDWI","SAVI","Interpolated data"),
                      name="")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none")

p4<-ggplot(dfs[[4]],aes(x=date,y=value))+
  geom_line(aes(colour=index))+
  geom_point(aes(colour=index),size=1.5)+
  geom_point(aes(colour=filled),size=2)+
  ylim(c(-0.25,1))+
  labs(x="Date",y="")+
  ggtitle("(D) Savanna VI downstream of SDM (2014-2018)")+
  scale_colour_manual(values=c("#a6cee3","#33a02c","#b2df8a","#1f78b4","#00000000","#a6611a","black"),
                      breaks=c("EVI","gNDVI","NDVI","NDWI","SAVI","Interpolated data"),
                      name="")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

p5<-ggplot(dfs[[5]],aes(x=date,y=value))+
  geom_line(aes(colour=index))+
  geom_point(aes(colour=index),size=1.5)+
  geom_point(aes(colour=filled),size=2)+
  labs(x="",y="Value")+
  ggtitle("Forest VI upstream of Serra da Mesa dam (1985-2012)")+
  scale_colour_manual(values=c("#a6cee3","#33a02c","#b2df8a","#1f78b4","#00000000","#a6611a","black"),
                      breaks=c("EVI","gNDVI","NDVI","NDWI","SAVI","Interpolated data"),
                      name="")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


p6<-ggplot(dfs[[6]],aes(x=date,y=value))+
  geom_line(aes(colour=index))+
  geom_point(aes(colour=index),size=1.5)+
  geom_point(aes(colour=filled),size=2)+
  labs(x="Date",y="Value")+
  ggtitle("Savanna VI upstream of Serra da Mesa dam (1985-2012)")+
  scale_colour_manual(values=c("#a6cee3","#33a02c","#b2df8a","#1f78b4","#00000000","#a6611a","black"),
                      breaks=c("EVI","gNDVI","NDVI","NDWI","SAVI","Interpolated data"),
                      name="")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

p7<-ggplot(dfs[[7]],aes(x=date,y=value))+
  geom_line(aes(colour=index))+
  geom_point(aes(colour=index),size=1.5)+
  geom_point(aes(colour=filled),size=2)+
  labs(x="",y="Value")+
  ggtitle("Forest VI downstream of Serra da Mesa dam (1985-2012)")+
  scale_colour_manual(values=c("#a6cee3","#33a02c","#b2df8a","#1f78b4","#00000000","#a6611a","black"),
                      breaks=c("EVI","gNDVI","NDVI","NDWI","SAVI","Interpolated data"),
                      name="")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

p8<-ggplot(dfs[[8]],aes(x=date,y=value))+
  geom_line(aes(colour=index))+
  geom_point(aes(colour=index),size=1.5)+
  geom_point(aes(colour=filled),size=2)+
  labs(x="Date",y="Value")+
  ggtitle("Savanna VI downstream of Serra da Mesa dam (1985-2012)")+
  scale_colour_manual(values=c("#a6cee3","#33a02c","#b2df8a","#1f78b4","#00000000","#a6611a","black"),
                      breaks=c("EVI","gNDVI","NDVI","NDWI","SAVI","Interpolated data"),
                      name="")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

(p1|p2)/(p3|p4)
ggsave("./Images/l8_VI.png",height=8,width=16)

p5/p6
ggsave("./Images/l5_VI_up.png",height=10,width=18)

p7/p8
ggsave("./Images/l5_VI_down.png",height=10,width=18)

dfs1 <- dfs[[1]] %>% filter(index=="EVI")

dfs1.fft <- fft(dfs1$value)

plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

plot.frequency.spectrum(dfs1.fft)

plot.harmonic <- function(Xk, i, ts, acq.freq, color="red") {
  Xk.h <- rep(0,length(Xk))
  Xk.h[i+1] <- Xk[i+1] # i-th harmonic
  harmonic.trajectory <- get.trajectory(Xk.h, ts, acq.freq=acq.freq)
  points(ts, harmonic.trajectory, type="l", col=color)
}

plot.harmonic(evi.dft,4,l8_for_up$date,15)
