#!/usr/bin/env Rscript
#### CRATION OF WEATHER TIME SERIES FOR GEOTOP 
####
#### @Author Emanuele Cordano 
####
###
library(meteobz)
library(geotopbricks)
library(zoo)
library(dplyr)
library(data.table)
####
source('/home/ecor/local/rpackages/rendena100/meteobz/R/get_data_from_zrx_file.R')
source('/home/ecor/local/rpackages/rendena100/meteobz/R/get_data_from_station.R')
####



wpath_meteo <- "/home/ecor/local/data/climate/south_tyrol/MeteoDataProvinceBZ/"
wpath_geotop <- "/home/ecor/local/repos/geotop_r_templink/geotop_simulations/templates/venosta_2024_00001/" ### TEST_Venosta_3D_034_template/" 
variables <- c("HeaderIPrec","HeaderWindVelocity","HeaderWindDirection","HeaderRH","HeaderAirTemp","HeaderSWglobal") %>% 
  get.geotop.inpts.keyword.value(wpath=wpath_geotop) %>% unlist()


geotop_meteo_stations <- get.geotop.inpts.keyword.value("MeteoStationCode",wpath=wpath_geotop,vector_sep=",")

print(variables)


tz <- "Etc/GMT-1"

meteo_new_ky <- get.geotop.inpts.keyword.value("MeteoFile",wpath=wpath_geotop,add_wpath=TRUE)




for (it in geotop_meteo_stations) {
  
  level <- which(geotop_meteo_stations==it)
   
  
   
  
   meteo_new <- get_data_from_station(var=variables,station=it,wpath=wpath_meteo,tz=tz,
                                      HeaderIPrec=variables["HeaderIPrec"],
                                      HeaderWindVelocity=variables["HeaderWindVelocity"],
                                      HeaderWindDirection=variables["HeaderWindDirection"],
                                      aggregation.timestep=3600,returns.as.zoo = TRUE,no.xy.windv=TRUE)
                                      
 
  
  
  file_prefix <- paste0(meteo_new_ky,"_NEW")
  create.geotop.meteo.files(x=meteo_new,file_prefix=file_prefix,level=level) 
  
  
}

## END
