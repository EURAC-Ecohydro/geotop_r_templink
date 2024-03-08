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

#wpath <- '/stablo001/local/simulations/venosta_2023/run/TEST_Venosta_3D_034/' 
#wpath <- '/home/ecor/local/geotop_simulations/Venosta_3D_036/'
##wpath <- '/home/ecor/local/geotop_simulations/venosta_2024_00001/'
wpath <- '/shared/data/Simulations2023/venosta_2024_00001_x_eurac/'
tz="Etc/GMT-1" ## check on geotop.inpts
###
start <-  get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",
                                         date=TRUE,wpath=wpath,tz=tz) 
end <- get.geotop.inpts.keyword.value("EndDateDDMMYYYYhhmm01Run",
                                      date=TRUE,wpath=wpath,tz=tz)-days(1)
###__###
val_theta <- seq(from=0,to=0.7,by=0.01)
pal_theta <- colorNumeric("YlGnBu",val_theta,na.color="#00000000") 
val_psi <- seq(from=-100*10^3,to=100*10^3,by=100)
pal_psi <- colorNumeric("YlGnBu",val_psi,na.color="#00000000")   ### YlGnBu RdYlBu
val_temp <- seq(from=-40,to=40,by=5)
pal_temp <- colorNumeric("RdYlBu",val_temp,reverse=TRUE,na.color="#00000000") 
val_snow <- seq(from=0,to=1000,by=10)
pal_snow <- colorNumeric("RdYlBu",val_snow,reverse=FALSE,na.color="#00000000") 
val_rad <- seq(from=0,to=4000,by=10)
pal_rad <- colorNumeric("RdYlBu",val_rad,reverse=TRUE,na.color="#00000000") 
val_iprec <- seq(from=0,to=200,by=10)
pal_iprec <- colorNumeric("YlGnBu",val_iprec,na.color="#00000000") 
###__###  BuGn
val_et <- seq(from=0,to=50,by=10)
pal_et <- colorNumeric("BuGn",val_et,na.color="#00000000")



###############
###############
###############
## Variable keywords (GEO-SPATIAL MAP)
variables  <- list(
  ##!SOIL
  
  
  ##
  ## SEE geotopbricks::brickFromOutputSoil3DTensor
  Liquid_Soil_Water_content=list(brickFromOutputSoil3DTensor=list(x="SoilLiqContentTensorFile",one.layer=FALSE,timestep = "OutputSoilMaps"), addLegend=list(pal=pal_theta,values=val_theta)),## addLegend(pal = pal, values = values(r),title = "Surface temp")
  Solid_Soil_Water_content=list(brickFromOutputSoil3DTensor=list(x="SoilIceContentTensorFile",one.layer=FALSE,timestep = "OutputSoilMaps"),addLegend=list(pal=pal_theta,values=val_theta)),
  Total_Soil_Water_pressure=list(brickFromOutputSoil3DTensor=list(x="SoilTotWaterPressTensorFile",one.layer=FALSE,timestep = "OutputSoilMaps"), addLegend=list(pal=pal_psi,values=val_psi)),  
 ## Liquid_Soil_Water_pressure=list(brickFromOutputSoil3DTensor=list(x="SoilLiqWaterPressTensorFile",one.layer=FALSE,timestep = "OutputSoilMaps"),addLegend=list(pal=pal_psi,values=val_psi)),   ##) ##= 
  Soil_Averaged_Temperature=list(brickFromOutputSoil3DTensor=list(x="SoilAveragedTempTensorFile",one.layer=FALSE,timestep = "OutputSoilMaps"),addLegend=list(pal=pal_temp,values=val_temp)),
  
 
 ## TO DO 
 
 # ! SNOW
  SWEMapFile=list(brickFromOutputSoil3DTensor=list(x="SWEMapFile",one.layer=TRUE,timestep="OutputSnowMaps"),addLegend=list(pal=pal_snow,values=val_snow)), ####= "output-maps/SWE"
  SnowDepthMapFile=list(brickFromOutputSoil3DTensor=list(x="SnowDepthMapFile",one.layer=TRUE,timestep="OutputSnowMaps"),addLegend=list(pal=pal_snow,values=val_snow)), #########=list(x="SnowDepthMapFile,one.layer=TRUE,timestep="OutputSnowMaps")  ###### = "output-maps/snowdepth"
  SnowMeltedMapFile=list(brickFromOutputSoil3DTensor=list(x="SnowMeltedMapFile",one.layer=TRUE,timestep="OutputSnowMaps"),addLegend=list(pal=pal_snow,values=val_snow)), #############  #SnowMeltedMapFile=list(x="SnowMeltedMapFile= "output-maps/snowmelt"
 # 
 # ! SURFACE
 # NetShortwaveRadiationMapFile = "output-maps/SWnet"
 # InShortwaveRadiationMapFile= "output-maps/SWin"
 # NetLongwaveRadiationMapFile = "output-maps/LWnet"
 # InLongwaveRadiationMapFile= "output-maps/LWin"
 # NetRadiationMapFile= "output-maps/RadNet"
 
 
  ## TO DO :
 # NetShortwaveRadiationMapFile=list(brickFromOutputSoil3DTensor=list(x="NetShortwaveRadiationMapFile",one.layer=TRUE,timestep=24),addLegend=list(pal=pal_rad,values=val_rad))
 
 
 
 
 ###
 # SurfaceHeatFluxMapFile = "output-maps/EB"
 # SurfaceSensibleHeatFluxMapFile = "output-maps/H"
 # SurfaceLatentHeatFluxMapFile = "output-maps/LE"
 # EvapotranspirationFromSoilMapFile = "output-maps/ET"
 # 
 
 EvapotranspirationFromSoilMapFile=list(brickFromOutputSoil3DTensor=list(x="EvapotranspirationFromSoilMapFile",one.layer=TRUE,timestep="OutputMeteoMaps"),addLegend=list(pal=pal_et,values=val_et)),
 
 # ! METEO
 # PrecipitationMapFile = "output-maps/Prec"
 # NetPrecipitationFile = "output-maps/Pnet" MISSING ? 
 # AirTempMapFile = "output-maps/Ta"
 ##OutputMeteoMaps
 AirTempMapFile=list(brickFromOutputSoil3DTensor=list(x="AirTempMapFile",one.layer=TRUE,timestep="OutputMeteoMaps"),addLegend=list(pal=pal_temp,values=val_temp)),
 NetPrecipitationFile=list(brickFromOutputSoil3DTensor=list(x="NetPrecipitationFile",one.layer=TRUE,timestep="OutputMeteoMaps"),addLegend=list(pal=pal_iprec,values=val_iprec)),
 PrecipitationMapFileTOTAL=list(brickFromOutputSoil3DTensor=list(x="PrecipitationMapFile",secondary.suffix="TOTAL",one.layer=TRUE,timestep="OutputMeteoMaps"),addLegend=list(pal=pal_iprec,values=val_iprec)),
 PrecipitationMapFileSNOW=list(brickFromOutputSoil3DTensor=list(x="PrecipitationMapFile",secondary.suffix="SNOW",one.layer=TRUE,timestep="OutputMeteoMaps"),addLegend=list(pal=pal_iprec,values=val_iprec))
 
### SnowMeltedMapFile=list(brickFromOutputSoil3DTensor=list(x="SnowMeltedMapFile",one.layer=TRUE,timestep="OutputSnowMaps"),addLegend=list(pal=pal_snow,values=val_snow))
 
 
)

variable_default <- "Total_Soil_Water_pressure"
# ### VARIABLE SOIL PROFILE 
# SoilTempProfileFile="output-tabs/soiltemp"
# SoilLiqContentProfileFile = "output-tabs/thetaliq" 
# SoilIceContentProfileFile = "output-tabs/thetaice" 
# SoilIceContentProfileFile = "output-tabs/thetaice" 
# SoilLiqWaterPressProfileFile = "output-tabs/psiliq"
# SoilTotWaterPressProfileFile = "output-tabs/psitot"
variables_profile <- c("SoilTempProfileFile","SoilLiqContentProfileFile","SoilIceContentProfileFile","SoilIceContentProfileFile",
                      "SoilLiqWaterPressProfileFile", 
                      "SoilTotWaterPressProfileFile")

names(variables_profile) <- variables_profile
variables_profile <- lapply(variables_profile,function(x){list(variable=x)})

variable_profile_default <- names(variables_profile)[1]

time_default <- start+seconds(as.numeric(end-start,unit="secs")*0.8)
time0_default <- start+seconds(as.numeric(end-start,unit="secs")*0.15)
basemaps <- c("OpenTopoMap","Esri.WorldImagery")

### WEATHER STATIONS
meteo <- get.geotop.points(prefix="MeteoStation",suffixes=c("Code","Name_DE","Name_IT","Elevation"),wpath=wpath)
meteo <- meteo %>% st_transform(crs=4326)




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


## CHECK POINTS
maxcrec <- 10
checkpoints <- get.geotop.points(prefix="CoordinatePoint",suffixes=c("ID","Name"),wpath=wpath)
checkpoints$ID <- as.numeric(checkpoints$CoordinatePointID)
checkpoints <- checkpoints %>% st_transform(crs=4326)
###
date_field_ckp <- "Date12.DDMMYYYYhhmm." ## It's a peculiarity of thias simulation.
checkpoint_key <- "PointOutputFile"
checkpoint_data <- get.geotop.inpts.keyword.value(checkpoint_key,date_field=date_field_ckp,wpath=wpath,data.frame=TRUE,
                                        level=1,tz=tz,ContinuousRecovery= maxcrec)

### Date12[DDMMYYYYhhmm],JulianDayFromYear0[days],
###TimeFromStart[days],Simulation_Period,Run,IDpoint,Psnow_over_canopy[mm],
### Prain_over_canopy[mm],Psnow_under_canopy[mm],Prain_under_canopy[mm],Prain_rain_on_snow[mm],
###Wind_speed[m/s],Wind_direction[deg],Relative_Humidity[-],Pressure[mbar],Tair[C],Tdew[C],Tsurface[C],
###Tvegetation[C],Tcanopyair[C],Surface_Energy_balance[W/m2],Soil_heat_flux[W/m2],SWin[W/m2],SWbeam[W/m2],SWdiff[W/m2],
###LWin[W/m2],LWin_min[W/m2],LWin_max[W/m2],SWnet[W/m2],LWnet[W/m2],H[W/m2],LE[W/m2],Canopy_fraction[-],LSAI[m2/m2],
###z0veg[m],d0veg[m],Estored_canopy[W/m2],SWv[W/m2],LWv[W/m2],Hv[W/m2],LEv[W/m2],Hg_unveg[W/m2],LEg_unveg[W/m2],
###Hg_veg[W/m2],LEg_veg[W/m2],Evap_surface[mm],Trasp_canopy[mm],Water_on_canopy[mm],Snow_on_canopy[mm],Qvegetation[-],
###Qsurface[-],Qair[-],Qcanopyair[-],LObukhov[m],LObukhovcanopy[m],Wind_speed_top_canopy[m/s],Decay_of_K_in_canopy[-],
####SWup[W/m2],LWup[W/m2],Hup[W/m2],LEup[W/m2],snow_depth[mm],snow_water_equivalent[mm],snow_density[kg/m3],snow_temperature[C],
####snow_melted[mm],snow_subl[mm],snow_blown_away[mm],snow_subl_while_blown[mm],glac_depth[mm],glac_water_equivalent[mm],glac_density[kg/m3],glac_temperature[C],glac_melted[mm],glac_subl[mm],lowest_thawed_soil_depth[mm],highest_thawed_soil_depth[mm],lowest_water_table_depth[mm],highest_water_table_depth[mm]
nn_checkpoint_vars <- names(checkpoint_data)
nn_checkpoint_vars_default <- nn_checkpoint_vars[str_detect(nn_checkpoint_vars,".mm.")]
# 

## DISCHARGE DATA
date_field_discharge <- "DATE.day.month.year.hour.min."
discharge_keyword <- "DischargeFile"
discharge_data <- get.geotop.inpts.keyword.value(discharge_keyword,date_field=date_field_discharge,wpath=wpath,data.frame=TRUE,
                                                  level=1,tz=tz,formatter = "") ## only one file
nn_discharge_vars <- names(discharge_data)
nn_discharge_vars_default <- nn_discharge_vars[str_detect(nn_discharge_vars,"Q")]
### BASIN FILE
date_field_basin <- "Date12.DDMMYYYYhhmm."
basin_keyword <- "BasinOutputFile"
basin_data <- get.geotop.inpts.keyword.value(basin_keyword,date_field=date_field_basin,wpath=wpath,data.frame=TRUE,
                                                 level=1,tz=tz,formatter = "") ## only one file
nn_basin_vars <- names(basin_data)
nn_basin_vars_default <- nn_basin_vars[str_detect(nn_basin_vars,".mm.")]

####

# 
# ! Tabs 
# DischargeFile = "output-tabs/discharge"
# 
# PointOutputFile = "output-tabs/point" 
# PointAll = 1
# 
# BasinOutputFile = "output-tabs/basin" 
# 
# 
## ADD ContinuousRecovery