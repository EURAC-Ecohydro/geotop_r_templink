library(dygraphs)
library(geotopbricks)

wpath <- '/stablo001/local/simulations/venosta_2023/run/TEST_Venosta_3D_034/' 
tz="Etc/GMT-1" ## check on geotop.inpts

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










## METEO HEADER 
nDate <- get.geotop.inpts.keyword.value("HeaderDateDDMMYYYYhhmmMeteo",wpath=wpath)
if(is.null(nDate)) nDate <- "Date"
#    HeaderDateDDMMYYYYhhmmMeteo="Date"
###
start <-  get.geotop.inpts.keyword.value("InitDateDDMMYYYYhhmm",
                                         date=TRUE,wpath=wpath,tz=tz) 
end <- get.geotop.inpts.keyword.value("EndDateDDMMYYYYhhmm",
                                      date=TRUE,wpath=wpath,tz=tz)-days(1)
###__###
dd <- list()
level=5
meteo_clicked <- meteo[level,]

meteodf <- get.geotop.inpts.keyword.value("MeteoFile",wpath=wpath,data.frame=TRUE,
                                          level=level,start=start,end=end,date_field = nDate,tz=tz)

for (nMeteoVar in nMeteoVars) {
  main <- sprintf("Variable %s vs time at %s/%s (%s) Elevation %s m ",nMeteoVar,
                  meteo_clicked$MeteoStationName_DE,meteo_clicked$MeteoStationName_IT,
                  meteo_clicked$MeteoStationCode,as.character(meteo_clicked$MeteoStationElevation))
  print(main)
  print(nMeteoVar)
  dd[[nMeteoVar]]  <- dygraph(meteodf[,nMeteoVar],main=main,ylab=nMeteoVar) %>% dyRangeSelector()
##  output[[paste0("dd",nMeteoVar)]] <- renderDygraph(udd)
}


### 
