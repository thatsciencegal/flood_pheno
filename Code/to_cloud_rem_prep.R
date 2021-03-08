## Code to stack Landsat images into a single GeoTIFF with all bands and to turn the Mapbiomas data into
## a mask for the area of interest.

library(raster)
library(rgdal)
library(ggplot2)
library(tidyverse)

## Set the in and out directories
in.dir <- "H:/SASA-PEAN/Raw/"
out.dir <- "H:/SASA-PEAN/Raw/Stacked/"

## List the files that you are using
l8.files <- list.files(in.dir, pattern="TIF$",full.names = TRUE)

## Split the file names so you can extract the date
l8.splt <- strsplit(l8.files, "_")

## Extract the date value for each list element
l8.splt.date <- as.character(unique(sapply(l8.splt,function(x) x[4])))

## Function to read in each unique data and stack the bands together into a single image
date.stack <- function(day){
  ## List files for a specific date
  l8.files1<-list.files(in.dir,pattern=paste0("LC08_L1TP_222069_",day,"*"),full.names = TRUE)
  ## Set the order for the bands
  ord.names <- c("B2","B3","B4","B5","B6")
  ##Create a raster stack of all the files
  rast <- stack(l8.files1)
  ## Rename the raster bands
  names(rast)<-c("B2","B3","B4","B5","B6")
  ## Re-order the raster to have the bands in the correct order
  ord.rast <- rast[[ord.names]]
}

## Read in each raster stack and save it as a single tif file
for(i in 1:length(l8.splt.date)){
  rast <- date.stack(l8.splt.date[i])
  writeRaster(rast,paste0(out.dir,l8.splt.date[i],".tif"),overwrite=TRUE)
}

## Upload Mapbiomas data for water mask
wtr.msk <- stack("D:/Dropbox/Dissertation/Chapter3/Data/MapbiomasVeg/sasa_pean_wtr_msk.tif")[[34]]

## Reproject the raster to match Landsat projection
wtr.msk.rpj <- projectRaster(wtr.msk,rast)

## Resample water mask to match Landsat's spatial properties
wtr.msk.rsp <- resample(wtr.msk.rc,rast)

## Reclassify water to 0 to create water mask
rcl <- c(33,0)
rcl.mtx <- matrix(rcl,ncol=2,byrow=TRUE)

wtr.msk.rc<-reclassify(wtr.msk.rsp,rcl.mtx)

## Mask out Landsat no data values on water mask
wtr.msk.rc.msk <- mask(wtr.msk.rsp,rast, maskvalue=0, updatevalue=NA)

## Write out water mask for year of interest
writeRaster(wtr.msk.rc.msk,"./Data/MapbiomasVeg/sasa_pean_wtr_msk_2018.tif",overwrite=TRUE)

## Stack all the raster files together
in.dir <- "H:/SASA-PEAN/Raw/Stacked"

## List rasters in the directory
l8.SS.PA<-list.files(in.dir,full.names=TRUE,pattern="tif$")

## Read each day as a raster stack
l8.ss.pa <- lapply(l8.SS.PA,stack)

## Initiate the raster that will be used for resampling
r <- l8.ss.pa[[1]]

## Resample all the rasters
for(i in 2:length(l8.ss.pa)){
  r1 <- l8.ss.pa[[i]]
  l8.ss.pa[[i]]<-resample(r1,r)
}

## Stack all the layers together
x <- stack()
for(i in 1:length(l8.ss.pa)){
  rast <- l8.ss.pa[[1]]
  x <- stack(x,rast)
}

## Write out the time series for the year
writeRaster(x,"H:/SASA-PEAN/Raw/Stacked/2018_sasa_pean.tif")
