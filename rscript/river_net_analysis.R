
## Network channal processing from a GEotop (www.geotop.org) simulation

# Author: Emanuele Cordano
# Date: 2023-11-18
# License: GPL-3.0

# Load necessary libraries
library(geotopbricks)
library(terra) ## using terra package version >=  1.7-59 (fork  devoleped by Emanuele Cordano)
library(magrittr)

geotop_tests <- "/home/ecor/local/sw/rendena100/geotop/tests/"
wpath <- "/home/ecor/local/repos/geotop_r_templink/geotop_simulations/templates/TEST_Venosta_3D_034_template/"

net <- get.geotop.inpts.keyword.value("RiverNetwork",raster=TRUE,wpath=wpath) 

### GEOTOP Examples
wpath_borden <-paste0(geotop_tests,"3D/Borden05m/")
net_borden <- get.geotop.inpts.keyword.value("RiverNetwork",raster=TRUE,wpath=wpath_borden) 
#
wpath_rendena <-paste0(geotop_tests,"3D/rendena/")
net_rendena <- get.geotop.inpts.keyword.value("RiverNetwork",raster=TRUE,wpath=wpath_rendena) 
##
wpath_uac <-paste0(geotop_tests,"3D/UpperAmmerCatchment/")
net_uac <- get.geotop.inpts.keyword.value("RiverNetwork",raster=TRUE,wpath=wpath_uac) 
##

elevation0 <- get.geotop.inpts.keyword.value("DemFile",raster=TRUE,wpath=wpath) %>% rast
net <- rast(net)
elevation <- elevation0+net10*0
###
###
flowdir <- terrain(elevation,"flowdir")
flowacc <- FlowAccu2(flowdir)+net*0
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
net_kw <- get.geotop.inpts.keyword.value("RiverNetwork",raster=FALSE,wpath=wpath,add_wpath=TRUE) 

for (it in names(out)) {
  
  filename <- paste(net_kw,it,sep="_")
  extension(filename) <- ".asc"
  writeRasterxGEOtop(raster(out[[it]]),filename=filename,overwrite = TRUE)
}
