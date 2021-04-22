library(lubridate)
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

## Make a data frame with day of year based on water year
doy <- data.frame(date=seq(as.Date("1984-07-01"),as.Date("2018-06-30"),by="day")) %>% 
  filter(!(month(date)==2 & day(date)==29)) %>% 
  mutate(doy = rep(1:365,times=34),
         WtrYr = year(as.Date(if_else(month(date)>=7, date+years(1), date),origin = "1984-01-01")))

for(i in 1:length(dfs)){
  dfs[[i]] <- dfs[[i]] %>% inner_join(doy,by="date")
}

## Combine data frames for entire time period
for.up <- bind_rows(dfs[[1]],dfs[[5]])
sav.up <- bind_rows(dfs[[2]],dfs[[6]])
for.sdm <- bind_rows(dfs[[3]],dfs[[7]])
sav.sdm <- bind_rows(dfs[[4]],dfs[[8]])

## Remove years with too much missing data and pivot data frame to wide
for.up1 <- for.up %>% filter(date < as.Date("1997-07-01")) %>% pivot_wider(names_from=index,values_from=value) 
for.up2 <- for.up %>% filter(date > as.Date("1998-06-30") & date < as.Date("2002-07-01")) %>% pivot_wider(names_from=index,values_from=value)
for.up3 <- for.up %>% filter(date > as.Date("2002-07-01") & date < as.Date("2010-07-01")) %>% pivot_wider(names_from=index,values_from=value)
for.up4 <- for.up %>% filter(date > as.Date("2013-07-01")) %>% pivot_wider(names_from=index,values_from=value)

sav.up1 <- sav.up %>% filter(date < as.Date("1997-07-01")) %>% pivot_wider(names_from=index,values_from=value)
sav.up2 <- sav.up %>% filter(date > as.Date("1998-06-30") & date < as.Date("2002-07-01")) %>% pivot_wider(names_from=index,values_from=value)
sav.up3 <- sav.up %>% filter(date > as.Date("2002-07-01") & date < as.Date("2010-07-01")) %>% pivot_wider(names_from=index,values_from=value)
sav.up4 <- sav.up %>% filter(date > as.Date("2013-07-01")) %>% pivot_wider(names_from=index,values_from=value)

for.sdm1 <- for.sdm %>% filter(date < as.Date("1997-07-01")) %>% pivot_wider(names_from=index,values_from=value)
for.sdm2 <- for.sdm %>% filter(date > as.Date("1998-06-30") & date < as.Date("2002-07-01")) %>% pivot_wider(names_from=index,values_from=value)
for.sdm3 <- for.sdm %>% filter(date > as.Date("2002-07-01") & date < as.Date("2010-07-01")) %>% pivot_wider(names_from=index,values_from=value)
for.sdm4 <- for.sdm %>% filter(date > as.Date("2013-07-01")) %>% pivot_wider(names_from=index,values_from=value)

sav.sdm1 <- sav.sdm %>% filter(date < as.Date("1997-07-01")) %>% pivot_wider(names_from=index,values_from=value)
sav.sdm2 <- sav.sdm %>% filter(date > as.Date("1998-06-30") & date < as.Date("2002-07-01")) %>% pivot_wider(names_from=index,values_from=value)
sav.sdm3 <- sav.sdm %>% filter(date > as.Date("2002-07-01") & date < as.Date("2010-07-01")) %>% pivot_wider(names_from=index,values_from=value)
sav.sdm4 <- sav.sdm %>% filter(date > as.Date("2013-07-01")) %>% pivot_wider(names_from=index,values_from=value)

##Discrete Fourier Transform
calc.fft <- function(tmp){
  ##Function to calculate fast Fourier and convert to inverse Fourier
  fft.nums <- fft(tmp)
  fft.nums[36:(length(fft.nums)-35)] <- 0+0i
  ift.nums <- fft(fft.nums,inverse=TRUE)/length(fft.nums)
  return(Re(ift.nums))
}

## Calcluate inverse Fourier for all the time periods, locations, and indices
for.up1$ndvi_ift <- calc.fft(for.up1$NDVI)
for.up1$gndvi_ift <- calc.fft(for.up1$gNDVI)
for.up1$evi_ift <- calc.fft(for.up1$EVI)
for.up1$savi_ift <- calc.fft(for.up1$SAVI)
for.up1$ndwi_ift <- calc.fft(for.up1$NDWI)

for.up2$ndvi_ift <- calc.fft(for.up2$NDVI)
for.up2$gndvi_ift <- calc.fft(for.up2$gNDVI)
for.up2$evi_ift <- calc.fft(for.up2$EVI)
for.up2$savi_ift <- calc.fft(for.up2$SAVI)
for.up2$ndwi_ift <- calc.fft(for.up2$NDWI)

for.up3$ndvi_ift <- calc.fft(for.up3$NDVI)
for.up3$gndvi_ift <- calc.fft(for.up3$gNDVI)
for.up3$evi_ift <- calc.fft(for.up3$EVI)
for.up3$savi_ift <- calc.fft(for.up3$SAVI)
for.up3$ndwi_ift <- calc.fft(for.up3$NDWI)

for.up4$ndvi_ift <- calc.fft(for.up4$NDVI)
for.up4$gndvi_ift <- calc.fft(for.up4$gNDVI)
for.up4$evi_ift <- calc.fft(for.up4$EVI)
for.up4$savi_ift <- calc.fft(for.up4$SAVI)
for.up4$ndwi_ift <- calc.fft(for.up4$NDWI)

sav.up1$ndvi_ift <- calc.fft(sav.up1$NDVI)
sav.up1$gndvi_ift <- calc.fft(sav.up1$gNDVI)
sav.up1$evi_ift <- calc.fft(sav.up1$EVI)
sav.up1$savi_ift <- calc.fft(sav.up1$SAVI)
sav.up1$ndwi_ift <- calc.fft(sav.up1$NDWI)

sav.up2$ndvi_ift <- calc.fft(sav.up2$NDVI)
sav.up2$gndvi_ift <- calc.fft(sav.up2$gNDVI)
sav.up2$evi_ift <- calc.fft(sav.up2$EVI)
sav.up2$savi_ift <- calc.fft(sav.up2$SAVI)
sav.up2$ndwi_ift <- calc.fft(sav.up2$NDWI)

sav.up3$ndvi_ift <- calc.fft(sav.up3$NDVI)
sav.up3$gndvi_ift <- calc.fft(sav.up3$gNDVI)
sav.up3$evi_ift <- calc.fft(sav.up3$EVI)
sav.up3$savi_ift <- calc.fft(sav.up3$SAVI)
sav.up3$ndwi_ift <- calc.fft(sav.up3$NDWI)

sav.up4$ndvi_ift <- calc.fft(sav.up4$NDVI)
sav.up4$gndvi_ift <- calc.fft(sav.up4$gNDVI)
sav.up4$evi_ift <- calc.fft(sav.up4$EVI)
sav.up4$savi_ift <- calc.fft(sav.up4$SAVI)
sav.up4$ndwi_ift <- calc.fft(sav.up4$NDWI)

for.sdm1$ndvi_ift <- calc.fft(for.sdm1$NDVI)
for.sdm1$gndvi_ift <- calc.fft(for.sdm1$gNDVI)
for.sdm1$evi_ift <- calc.fft(for.sdm1$EVI)
for.sdm1$savi_ift <- calc.fft(for.sdm1$SAVI)
for.sdm1$ndwi_ift <- calc.fft(for.sdm1$NDWI)

for.sdm2$ndvi_ift <- calc.fft(for.sdm2$NDVI)
for.sdm2$gndvi_ift <- calc.fft(for.sdm2$gNDVI)
for.sdm2$evi_ift <- calc.fft(for.sdm2$EVI)
for.sdm2$savi_ift <- calc.fft(for.sdm2$SAVI)
for.sdm2$ndwi_ift <- calc.fft(for.sdm2$NDWI)

for.sdm3$ndvi_ift <- calc.fft(for.sdm3$NDVI)
for.sdm3$gndvi_ift <- calc.fft(for.sdm3$gNDVI)
for.sdm3$evi_ift <- calc.fft(for.sdm3$EVI)
for.sdm3$savi_ift <- calc.fft(for.sdm3$SAVI)
for.sdm3$ndwi_ift <- calc.fft(for.sdm3$NDWI)

for.sdm4$ndvi_ift <- calc.fft(for.sdm4$NDVI)
for.sdm4$gndvi_ift <- calc.fft(for.sdm4$gNDVI)
for.sdm4$evi_ift <- calc.fft(for.sdm4$EVI)
for.sdm4$savi_ift <- calc.fft(for.sdm4$SAVI)
for.sdm4$ndwi_ift <- calc.fft(for.sdm4$NDWI)

sav.sdm1$ndvi_ift <- calc.fft(sav.sdm1$NDVI)
sav.sdm1$gndvi_ift <- calc.fft(sav.sdm1$gNDVI)
sav.sdm1$evi_ift <- calc.fft(sav.sdm1$EVI)
sav.sdm1$savi_ift <- calc.fft(sav.sdm1$SAVI)
sav.sdm1$ndwi_ift <- calc.fft(sav.sdm1$NDWI)

sav.sdm2$ndvi_ift <- calc.fft(sav.sdm2$NDVI)
sav.sdm2$gndvi_ift <- calc.fft(sav.sdm2$gNDVI)
sav.sdm2$evi_ift <- calc.fft(sav.sdm2$EVI)
sav.sdm2$savi_ift <- calc.fft(sav.sdm2$SAVI)
sav.sdm2$ndwi_ift <- calc.fft(sav.sdm2$NDWI)

sav.sdm3$ndvi_ift <- calc.fft(sav.sdm3$NDVI)
sav.sdm3$gndvi_ift <- calc.fft(sav.sdm3$gNDVI)
sav.sdm3$evi_ift <- calc.fft(sav.sdm3$EVI)
sav.sdm3$savi_ift <- calc.fft(sav.sdm3$SAVI)
sav.sdm3$ndwi_ift <- calc.fft(sav.sdm3$NDWI)

sav.sdm4$ndvi_ift <- calc.fft(sav.sdm4$NDVI)
sav.sdm4$gndvi_ift <- calc.fft(sav.sdm4$gNDVI)
sav.sdm4$evi_ift <- calc.fft(sav.sdm4$EVI)
sav.sdm4$savi_ift <- calc.fft(sav.sdm4$SAVI)
sav.sdm4$ndwi_ift <- calc.fft(sav.sdm4$NDWI)

## Do fft across the entire dataset
for.sdm.wide <- for.sdm %>% filter(!is.na(value),
                                 date < as.Date("2002-08-23") | date > as.Date("2003-06-23")) %>% 
                          pivot_wider(names_from = index)

for.sdm.wide$ndvi_ift <- calc.fft(for.sdm.wide$NDVI)
for.sdm.wide$gndvi_ift <- calc.fft(for.sdm.wide$gNDVI)
for.sdm.wide$ndwi_ift <- calc.fft(for.sdm.wide$NDWI)
for.sdm.wide$evi_ift <- calc.fft(for.sdm.wide$EVI)
for.sdm.wide$savi_ift <- calc.fft(for.sdm.wide$SAVI)

ggplot(for.sdm.wide,aes(x=date,y=ndwi_ift))+geom_line()+geom_line(aes(y=NDWI),color="blue",linetype="dashed")
ggplot(for.sdm.wide,aes(x=date,y=evi_ift))+geom_line()+geom_line(aes(y=EVI),color="blue",linetype="dashed")
ggplot(for.sdm.wide,aes(x=date,y=savi_ift))+geom_line()+geom_line(aes(y=SAVI),color="blue",linetype="dashed")

sav.sdm.wide <- sav.sdm %>% filter(!is.na(value),
                                 date < as.Date("2002-08-23") | date > as.Date("2003-06-23")) %>% 
  pivot_wider(names_from = index)

sav.sdm.wide$ndvi_ift <- calc.fft(sav.sdm.wide$NDVI)
sav.sdm.wide$gndvi_ift <- calc.fft(sav.sdm.wide$gNDVI)
sav.sdm.wide$ndwi_ift <- calc.fft(sav.sdm.wide$NDWI)
sav.sdm.wide$evi_ift <- calc.fft(sav.sdm.wide$EVI)
sav.sdm.wide$savi_ift <- calc.fft(sav.sdm.wide$SAVI)

ggplot(sav.sdm.wide,aes(x=date,y=ndwi_ift))+geom_line()+geom_line(aes(y=NDWI),color="blue",linetype="dashed")
ggplot(sav.sdm.wide,aes(x=date,y=evi_ift))+geom_line()+geom_line(aes(y=EVI),color="blue",linetype="dashed")
ggplot(sav.sdm.wide,aes(x=date,y=savi_ift))+geom_line()+geom_line(aes(y=SAVI),color="blue",linetype="dashed")

for.up.wide <- for.up %>% filter(!is.na(value),
                                 date < as.Date("2002-08-23") | date > as.Date("2003-06-23")) %>% 
  pivot_wider(names_from = index)

for.up.wide$ndvi_ift <- calc.fft(for.up.wide$NDVI)
for.up.wide$gndvi_ift <- calc.fft(for.up.wide$gNDVI)
for.up.wide$ndwi_ift <- calc.fft(for.up.wide$NDWI)
for.up.wide$evi_ift <- calc.fft(for.up.wide$EVI)
for.up.wide$savi_ift <- calc.fft(for.up.wide$SAVI)

ggplot(for.up.wide,aes(x=date,y=ndwi_ift))+geom_line()+geom_line(aes(y=NDWI),color="blue",linetype="dashed")
ggplot(for.up.wide,aes(x=date,y=evi_ift))+geom_line()+geom_line(aes(y=EVI),color="blue",linetype="dashed")
ggplot(for.up.wide,aes(x=date,y=savi_ift))+geom_line()+geom_line(aes(y=SAVI),color="blue",linetype="dashed")

sav.up.wide <- sav.up %>% filter(!is.na(value),
                                 date < as.Date("2002-08-23") | date > as.Date("2003-06-23")) %>% 
  pivot_wider(names_from = index)

sav.up.wide$ndvi_ift <- calc.fft(sav.up.wide$NDVI)
sav.up.wide$gndvi_ift <- calc.fft(sav.up.wide$gNDVI)
sav.up.wide$ndwi_ift <- calc.fft(sav.up.wide$NDWI)
sav.up.wide$evi_ift <- calc.fft(sav.up.wide$EVI)
sav.up.wide$savi_ift <- calc.fft(sav.up.wide$SAVI)

ggplot(sav.up.wide,aes(x=date,y=ndwi_ift))+geom_line()+geom_line(aes(y=NDWI),color="blue",linetype="dashed")
ggplot(sav.up.wide,aes(x=date,y=evi_ift))+geom_line()+geom_line(aes(y=EVI),color="blue",linetype="dashed")
ggplot(sav.up.wide,aes(x=date,y=savi_ift))+geom_line()+geom_line(aes(y=SAVI),color="blue",linetype="dashed")

## Summarise annual maximum and minimum
for.up.min <- for.up.wide %>% select(doy.x,ndwi_ift,evi_ift,savi_ift,WtrYr) %>%  
  pivot_longer(cols=!c(doy.x,WtrYr),names_to="index",values_to="value") %>% 
  group_by(WtrYr,index) %>% 
  filter(value==min(value)) %>% 
  mutate(prepos=case_when(WtrYr < 1998 ~ "Pre",
                          WtrYr >= 1998 ~ "Post"))
 

for.up.max <- for.up.wide %>% select(doy.x,ndwi_ift,evi_ift,savi_ift,WtrYr) %>% 
  pivot_longer(cols=!c(doy.x,WtrYr),names_to="index",values_to="value") %>% 
  group_by(WtrYr,index) %>% 
  filter(value==max(value)) %>% 
  mutate(prepos=case_when(WtrYr < 1998 ~ "Pre",
                          WtrYr >= 1998 ~ "Post"))

ggplot(for.up.min)+
  geom_density(aes(x=doy.x,fill=index),position="identity",alpha=0.7)+
  scale_fill_manual(values=c("#7fcdbb","#225ea8","#edf8b1"),
                    labels=c("EVI","NDWI","SAVI"))+
  labs(x="Day of year",y="Count",fill="VI")+
  ggtitle("DOY of minimum VI, upstream forest")

ggplot(for.up.max)+
  geom_density(aes(x=doy.x,fill=index),position="identity",alpha=0.7)+
  scale_fill_manual(values=c("#7fcdbb","#225ea8","#edf8b1"),
                    labels=c("EVI","NDWI","SAVI"))+
  labs(x="Day of year",y="Count",fill="VI")+
  ggtitle("DOY of maximum VI, upstream forest")

ggplot(for.up.min)+
  geom_boxplot(aes(x=index,y=doy.x,fill=prepos),alpha=0.7)+
  scale_fill_manual(values=c("#7fcdbb","#edf8b1"),
                    labels=c("Pre","Post"))+
  labs(x="Vegetation index",y="Day of year",fill="Dam status")+
  scale_x_discrete(breaks=c("evi_ift","ndwi_ift","savi_ift"),
                   labels=c("EVI","NDWI","SAVI"))+
  ggtitle("DOY of minimum VI, upstream forest")

ggplot(for.up.max)+
  geom_boxplot(aes(x=index,y=doy.x,fill=prepos),alpha=0.7)+
  scale_fill_manual(values=c("#7fcdbb","#edf8b1"),
                    labels=c("Pre","Post"))+
  labs(x="Vegetation index",y="Day of year",fill="Dam status")+
  scale_x_discrete(breaks=c("evi_ift","ndwi_ift","savi_ift"),
                   labels=c("EVI","NDWI","SAVI"))+
  ggtitle("DOY of minimum VI, upstream forest")

sav.up.min <- sav.up.wide %>% select(doy.x,ndwi_ift,evi_ift,savi_ift,WtrYr) %>% 
  pivot_longer(cols=!c(doy.x,WtrYr),names_to="index",values_to="value") %>% 
  group_by(WtrYr,index) %>% 
  filter(value==min(value)) %>% 
  mutate(prepos=case_when(WtrYr < 1998 ~ "Pre",
                          WtrYr >= 1998 ~ "Post"))


sav.up.max <- sav.up.wide %>% select(doy.x,ndwi_ift,evi_ift,savi_ift,WtrYr) %>% 
  pivot_longer(cols=!c(doy.x,WtrYr),names_to="index",values_to="value") %>% 
  group_by(WtrYr,index) %>% 
  filter(value==max(value)) %>% 
  mutate(prepos=case_when(WtrYr < 1998 ~ "Pre",
                          WtrYr >= 1998 ~ "Post"))

ggplot(sav.up.min)+
  geom_boxplot(aes(x=index,y=doy.x,fill=prepos),alpha=0.7)+
  scale_fill_manual(values=c("#7fcdbb","#edf8b1"),
                    labels=c("Pre","Post"))+
  labs(x="Vegetation index",y="Day of year",fill="Dam status")+
  scale_x_discrete(breaks=c("evi_ift","ndwi_ift","savi_ift"),
                   labels=c("EVI","NDWI","SAVI"))+
  ggtitle("DOY of minimum VI, upstream savanna")

ggplot(sav.up.max)+
  geom_boxplot(aes(x=index,y=doy.x,fill=prepos),alpha=0.7)+
  scale_fill_manual(values=c("#7fcdbb","#edf8b1"),
                    labels=c("Pre","Post"))+
  labs(x="Vegetation index",y="Day of year",fill="Dam status")+
  scale_x_discrete(breaks=c("evi_ift","ndwi_ift","savi_ift"),
                   labels=c("EVI","NDWI","SAVI"))+
  ggtitle("DOY of maximum VI, upstream savanna")


for.sdm.min <- for.sdm.wide %>% select(doy.x,ndwi_ift,evi_ift,savi_ift,WtrYr) %>%  
  pivot_longer(cols=!c(doy.x,WtrYr),names_to="index",values_to="value") %>% 
  group_by(WtrYr,index) %>% 
  filter(value==min(value)) %>% 
  mutate(prepos=case_when(WtrYr < 1998 ~ "Pre",
                          WtrYr >= 1998 ~ "Post"))


for.sdm.max <- for.sdm.wide %>% select(doy.x,ndwi_ift,evi_ift,savi_ift,WtrYr) %>% 
  pivot_longer(cols=!c(doy.x,WtrYr),names_to="index",values_to="value") %>% 
  group_by(WtrYr,index) %>% 
  filter(value==max(value)) %>% 
  mutate(prepos=case_when(WtrYr < 1998 ~ "Pre",
                          WtrYr >= 1998 ~ "Post"))

ggplot(for.sdm.min)+
  geom_density(aes(x=doy.x,fill=index),position="identity",alpha=0.7)+
  scale_fill_manual(values=c("#7fcdbb","#225ea8","#edf8b1"),
                    labels=c("EVI","NDWI","SAVI"))+
  labs(x="Day of year",y="Count",fill="VI")+
  ggtitle("DOY of minimum VI, upstream forest")

ggplot(for.sdm.max)+
  geom_density(aes(x=doy.x,fill=index),position="identity",alpha=0.7)+
  scale_fill_manual(values=c("#7fcdbb","#225ea8","#edf8b1"),
                    labels=c("EVI","NDWI","SAVI"))+
  labs(x="Day of year",y="Count",fill="VI")+
  ggtitle("DOY of maximum VI, upstream forest")

ggplot(for.sdm.min)+
  geom_boxplot(aes(x=index,y=doy.x,fill=prepos),alpha=0.7)+
  scale_fill_manual(values=c("#7fcdbb","#edf8b1"),
                    labels=c("Pre","Post"))+
  labs(x="Vegetation index",y="Day of year",fill="Dam status")+
  scale_x_discrete(breaks=c("evi_ift","ndwi_ift","savi_ift"),
                   labels=c("EVI","NDWI","SAVI"))+
  ggtitle("DOY of minimum VI, downstream forest")

ggplot(for.sdm.max)+
  geom_boxplot(aes(x=index,y=doy.x,fill=prepos),alpha=0.7)+
  scale_fill_manual(values=c("#7fcdbb","#edf8b1"),
                    labels=c("Pre","Post"))+
  labs(x="Vegetation index",y="Day of year",fill="Dam status")+
  scale_x_discrete(breaks=c("evi_ift","ndwi_ift","savi_ift"),
                   labels=c("EVI","NDWI","SAVI"))+
  ggtitle("DOY of minimum VI, downstream forest")

sav.sdm.min <- sav.sdm.wide %>% select(doy.x,ndwi_ift,evi_ift,savi_ift,WtrYr) %>% 
  pivot_longer(cols=!c(doy.x,WtrYr),names_to="index",values_to="value") %>% 
  group_by(WtrYr,index) %>% 
  filter(value==min(value)) %>% 
  mutate(prepos=case_when(WtrYr < 1998 ~ "Pre",
                          WtrYr >= 1998 ~ "Post"))


sav.sdm.max <- sav.sdm.wide %>% select(doy.x,ndwi_ift,evi_ift,savi_ift,WtrYr) %>% 
  pivot_longer(cols=!c(doy.x,WtrYr),names_to="index",values_to="value") %>% 
  group_by(WtrYr,index) %>% 
  filter(value==max(value)) %>% 
  mutate(prepos=case_when(WtrYr < 1998 ~ "Pre",
                          WtrYr >= 1998 ~ "Post"))

ggplot(sav.sdm.min)+
  geom_boxplot(aes(x=index,y=doy.x,fill=prepos),alpha=0.7)+
  scale_fill_manual(values=c("#7fcdbb","#edf8b1"),
                    labels=c("Pre","Post"))+
  labs(x="Vegetation index",y="Day of year",fill="Dam status")+
  scale_x_discrete(breaks=c("evi_ift","ndwi_ift","savi_ift"),
                   labels=c("EVI","NDWI","SAVI"))+
  ggtitle("DOY of minimum VI, downstream savanna")

ggplot(sav.sdm.max)+
  geom_boxplot(aes(x=index,y=doy.x,fill=prepos),alpha=0.7)+
  scale_fill_manual(values=c("#7fcdbb","#edf8b1"),
                    labels=c("Pre","Post"))+
  labs(x="Vegetation index",y="Day of year",fill="Dam status")+
  scale_x_discrete(breaks=c("evi_ift","ndwi_ift","savi_ift"),
                   labels=c("EVI","NDWI","SAVI"))+
  ggtitle("DOY of maximum VI, downstream savanna")

f.up.min.mod <- anova(lm(doy.x~prepos+index,data=for.up.min))
f.up.max.mod <- anova(lm(doy.x~prepos+index,data=for.up.max))

s.up.min.mod <- anova(lm(doy.x~prepos+index,data=sav.up.min))
s.up.max.mod <- anova(lm(doy.x~prepos+index,data=sav.up.max))

f.sdm.min.mod <- anova(lm(doy.x~prepos+index,data=for.sdm.min))
f.sdm.max.mod <- anova(lm(doy.x~prepos+index,data=for.sdm.max))

s.sdm.min.mod <- anova(lm(doy.x~prepos+index,data=sav.sdm.min))
s.sdm.max.mod <- anova(lm(doy.x~prepos+index,data=sav.sdm.max))
## Raw data plots
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