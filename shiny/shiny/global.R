# 
# App Shiny - GEOtop Interface
# Author: Emanuele Cordano
# Date: October 2023
# License: GPL-3
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
#

library(shiny)


library(geotopbricks)
library(lubridate)
library(leaflet)
library(sf)
library(data.table)
library(dplyr)
library(dygraphs)
library(zoo)

wpath <- '/stablo001/local/simulations/venosta_2023/run/TEST_Venosta_3D_034/' 
tz="Etc/GMT-1" ## check on geotop.inpts
###
start <-  get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",
                                         date=TRUE,wpath=wpath,tz=tz) 
end <- get.geotop.inpts.keyword.value("EndDateDDMMYYYYhhmm",
                                      date=TRUE,wpath=wpath,tz=tz)-days(1)
###__###
val_theta <- seq(from=0,to=0.7,by=0.01)
pal_theta <- colorNumeric("YlGnBu",val_theta,na.color="#00000000") 
val_psi <- seq(from=-100*10^3,to=100*10^3,by=100)
pal_psi <- colorNumeric("YlGnBu",val_psi,na.color="#00000000")   ### YlGnBu RdYlBu
val_temp <- seq(from=-40,to=40,by=5)
pal_temp <- colorNumeric("RdYlBu",val_temp,reverse=TRUE,na.color="#00000000") 
###__###


## Variable keywords
variables  <- list(
  ##!SOIL
  
  
  ##
  ## SEE geotopbricks::brickFromOutputSoil3DTensor
  Liquid_Soil_Water_content=list(brickFromOutputSoil3DTensor=list(x="SoilLiqContentTensorFile",one.layer=FALSE,timestep = "OutputSoilMaps"), addLegend=list(pal=pal_theta,values=val_theta)),## addLegend(pal = pal, values = values(r),title = "Surface temp")
  Solid_Soil_Water_content=list(brickFromOutputSoil3DTensor=list(x="SoilIceContentTensorFile",one.layer=FALSE,timestep = "OutputSoilMaps"),addLegend=list(pal=pal_theta,values=val_theta)),
  Total_Soil_Water_pressure=list(brickFromOutputSoil3DTensor=list(x="SoilTotWaterPressTensorFile",one.layer=FALSE,timestep = "OutputSoilMaps"), addLegend=list(pal=pal_psi,values=val_psi)),  
 ## Liquid_Soil_Water_pressure=list(brickFromOutputSoil3DTensor=list(x="SoilLiqWaterPressTensorFile",one.layer=FALSE,timestep = "OutputSoilMaps"),addLegend=list(pal=pal_psi,values=val_psi)),   ##) ##= 
  Soil_Averaged_Temperature=list(brickFromOutputSoil3DTensor=list(x="SoilAveragedTempTensorFile",one.layer=FALSE,timestep = "OutputSoilMaps"),addLegend=list(pal=pal_temp,values=val_temp))
  
)

variable_default <- "Total_Soil_Water_pressure"

time_default <- start+seconds(as.numeric(end-start,unit="secs")*0.8)
time0_default <- start+seconds(as.numeric(end-start,unit="secs")*0.15)
basemaps <- c("OpenTopoMap","Esri.WorldImagery")


meteo <- get.geotop.points(prefix="MeteoStation",suffix=c("Code","Name_DE","Name_IT","Elevation"),wpath=wpath)
meteo <- meteo %>% st_transform(crs=4326)
### WEATHER STATIONS


## Reactive values


# ## boolean option It is 1 when App is launching, it becomes FALSE during its execution. 
### LEAFLET INITALIZATION 
##


bbox <- st_bbox(meteo)
west <- bbox$xmin %>% as.numeric()
south <- bbox$ymin %>% as.numeric()
east <- bbox$xmax %>% as.numeric()
north <- bbox$ymax %>% as.numeric()
 
## METEO HEADER 
nDate <- get.geotop.inpts.keyword.value("HeaderDateDDMMYYYYhhmmMeteo",wpath=wpath)
if(is.null(nDate)) nDate <- "Date"
#    HeaderDateDDMMYYYYhhmmMeteo="Date"



#    HeaderIPrec="N"
nIPrec <- get.geotop.inpts.keyword.value("HeaderIPrec",wpath=wpath)
if(is.null(nIPrec)) nIPrec <- "IPrec"


#    HeaderWindVelocity="WG"
nWindVelocity <- get.geotop.inpts.keyword.value("HeaderWindVelocity",wpath=wpath)
if(is.null(nWindVelocity)) nWindVelocity <- "WindVelocity"


#    HeaderWindDirection="WR"
nWindDirection <- get.geotop.inpts.keyword.value("HeaderWindDirection",wpath=wpath)
if(is.null(nWindDirection)) nWindDirection <- "WindDirection"

#    HeaderRH="LF"
nRH <- get.geotop.inpts.keyword.value("HeaderRH",wpath=wpath)
if(is.null(nRH)) nRH <- "RH"

#    HeaderAirTemp="LT"
nAirTemp <- get.geotop.inpts.keyword.value("HeaderAirTemp",wpath=wpath)
if(is.null(nAirTemp)) nAirTemp <- "AirTemp"

#    HeaderSWglobal="GS"
nSWglobal <- get.geotop.inpts.keyword.value("HeaderSWglobal",wpath=wpath)
if(is.null(nSWglobal)) nSWglobal <- "nSWglobal"


#    HeaderCloudSWTransmissivity="CloudTrans"
nCloudTrans <- get.geotop.inpts.keyword.value("HeaderCloudSWTransmissivity",wpath=wpath)
if(is.null(nCloudTrans)) nCloudTrans <- "nCloudTrans"

nMeteoVars <- c(nIPrec,nAirTemp,nRH,nWindVelocity,nWindDirection,nSWglobal,nCloudTrans)