
## Network channal processing from a GEotop (www.geotop.org) simulation

# Author: Emanuele Cordano
# Date: 2024-01-02
# License: GPL-3.0

# Load necessary libraries
library(geotopbricks)
library(terra) ## using terra package version >=  1.7-59 (fork  devoloped by Emanuele Cordano) (https://github.com/ecor/terra)
library(magrittr)

geotop_tests <- "/home/ecor/local/sw/rendena100/geotop/tests/"
wpath <- "/home/ecor/local/repos/geotop_r_templink/geotop_simulations/templates/venosta_2024_00001/"  ####TEST_Venosta_3D_034_template/"
#wpath <- '/home/ecor/local/geotop_simulations/Venosta_3D_036_template/'
net <- get.geotop.inpts.keyword.value("OldRiverNetwork",raster=TRUE,wpath=wpath) %>% rast()
landcover <- get.geotop.inpts.keyword.value("OldLandCoverMapFile",raster=TRUE,wpath=wpath) %>% rast()
elevation0 <- get.geotop.inpts.keyword.value("DemFile",raster=TRUE,wpath=wpath) %>% rast()
landcover_with_net <- landcover
landcover_with_net[net!=0] <- 12
elevation <- elevation0+net*0
###
###
flowdir <- terrain(elevation,"flowdir")
flowacc <- flowAccu2(flowdir)+net*0
pourpoint <- (flowacc==max(flowacc[],na.rm=TRUE))
##net_configuraions 
out <- list()
out$v_00_10_11 <- net+pourpoint
out$v_00_00_01 <- net*0+pourpoint

### See Gracomo's email: 
#L´unica é lanciare 2 simulazioni di tests.
#
#senza rete e con solo 1 all´uscita
#con rete ma con 11 all´uscita
net_kw <- get.geotop.inpts.keyword.value("OldRiverNetwork",raster=FALSE,wpath=wpath,add_wpath=TRUE) 
land_kw <- get.geotop.inpts.keyword.value("OldLandCoverMapFile",raster=FALSE,wpath=wpath,add_wpath=TRUE) 
for (it in names(out)) {
  
  filename <- paste(net_kw,it,sep="_")
  extension(filename) <- ".asc"
  writeRasterxGEOtop(raster(out[[it]]),filename=filename,overwrite = TRUE)
}

filename <- paste(land_kw,"with_net",sep="_")
extension(filename) <- ".asc"
writeRasterxGEOtop(raster(landcover_with_net),filename=filename,overwrite = TRUE)