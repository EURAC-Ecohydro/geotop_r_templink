

library(geotopbricks)
## Not run: 
# The data containing in the link are only for educational use
wpath <- 'https://raw.githubusercontent.com/ecor/geotopbricks_doc/master/simulations/idroclim_test1'
wpath <- '/stablo001/local/simulations/venosta_2023/run/TEST_Venosta_3D_034/' 
## URL path (RAW VERSION) of 
## https://github.com/ecor/geotopbricks_doc/tree/master/simulations/idroclim_test1
x <- "SoilLiqContentTensorFile"
tz <-  "Etc/GMT-1"
when <- as.POSIXct("2002-03-22",tz=tz)
when <- as.POSIXct("2020-01-22",tz=tz)
# Not Run because it elapses too long time!!! 
# Please Uncomment the following lines to run by yourself!!!
b <- brickFromOutputSoil3DTensor(x,when=when,wpath=wpath,tz=tz,use.read.raster.from.url=TRUE)

# a 2D map: 
x_e <- "SnowDepthMapFile"
# Not Run: uncomment the following line

m <- rasterFromOutput2DMap(x_e,when=when,wpath=wpath,timestep="OutputSnowMaps",
                           tz=tz,use.read.raster.from.url=TRUE)
## NOTE: set use.read.raster.from.url=FALSE (default) 
# if the "wpath" directorty is in the local file system.
# Not Run: uncomment the following line
plot(m)

## End(Not run)