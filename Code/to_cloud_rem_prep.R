## Code to stack Landsat images into a single GeoTIFF with all bands and to turn the Mapbiomas data into
## a mask for the area of interest.

library(raster)

## Set the in and out directories
in.dir <- "H:/SASA-PEAN/Raw"
out.dir <- "H:/SASA-PEAN/Raw/Stacked"

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
  ord.names <- c("B1","B2","B3","B4","B5","B6","B7","B9","B10","B11")
  ##Create a raster stack of all the files
  rast <- stack(l8.files1)
  ## Rename the raster bands
  names(rast)<-c("B1","B10","B11","B2","B3","B4","B5","B6","B7","B9")
  ## Re-order the raster to have the bands in the correct order
  ord.rast <- rast[[ord.names]]
}

## Read in each raster stack and save it as a single tif file
for(i in 1:length(l8.splt.date)){
  rast <- date.stack(l8.splt.date[i])
  writeRaster(rast,paste0(out.dir,day,".tif"),overwrite=TRUE)
}
