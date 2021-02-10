library(raster)

## Read in vegetation in the 500 m buffer around the river
to.veg <- stack('./Data/MapbiomasVeg/to_buff.tif')[[-35]]

## Rename layers in to.veg to reflect years
names(to.veg) <- c('to_1985','to_1986','to_1987','to_1988','to_1989',
                   'to_1990','to_1991','to_1992','to_1993','to_1994',
                   'to_1995','to_1996','to_1997','to_1998','to_1999',
                   'to_2000','to_2001','to_2002','to_2003','to_2004',
                   'to_2005','to_2006','to_2007','to_2008','to_2009',
                   'to_2010','to_2011','to_2012','to_2013','to_2014',
                   'to_2015','to_2016','to_2017','to_2018')

## Make reclassifications matrices for forest and savanna types (where forest = 1 and all other lc = 0 and where savanna = 1 and all other lc = 0)
forest <- c(1,0,2,0,3,1,4,0,5,0,9,0,10,0,11,0,12,0,32,0,29,0,13,0,14,0,15,0,18,0,19,0,20,0,21,0,22,0,23,0,24,0,30,0,
            25,0,26,0,33,0,31,0,27,0)

rc.for <- matrix(forest,ncol=2,byrow=TRUE)

savanna <- c(1,0,2,0,3,0,4,1,5,0,9,0,10,0,11,0,12,0,32,0,29,0,13,0,14,0,15,0,18,0,19,0,20,0,21,0,22,0,23,0,24,0,30,0,
             25,0,26,0,33,0,31,0,27,0)

rc.sav <- matrix(savanna,ncol=2,byrow=TRUE)

## Reclassify forest and savanna to make masks
to.forest <- reclassify(to.veg,rc.for,right=NA)
to.savanna <- reclassify(to.veg,rc.sav,right=NA)

## Add up all forest and savanna pixels to generate which pixels remained forest/savanna for the whole study (n=34)
to.for.tot <- sum(to.forest)
to.sav.tot <- sum(to.savanna)

## Reclassify the pixels to show only those which stayed forest/savanna for the entire time period
all.pix <- c(-1,33,0,33,34,1)
pix.mat <- matrix(all.pix,ncol=3,byrow=TRUE)

stay.for <- reclassify(to.for.tot,pix.mat,right=TRUE)
stay.sav <- reclassify(to.sav.tot,pix.mat,right=TRUE)

## Write out rasters
writeRaster(stay.for,"./Data/Reclassified/tocantins_forest.tif")
writeRaster(stay.sav,"./Data/Reclassified/tocantins_savanna.tif")
