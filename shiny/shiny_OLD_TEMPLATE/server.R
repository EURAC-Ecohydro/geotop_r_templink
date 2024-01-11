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

source('./global.R')










# Define server logic required to draw a histogram
function(input, output, session) {

  output$meteo_station_table <- renderTable({
    if (input$meteo) {
      print(meteo)
      meteo %>% as.data.frame() %>% select(-geometry)
    } else {
      NULL
    }  
    })
  

  output$map2 <- renderLeaflet({ 
    outleaf <- leaflet() %>% addProviderTiles(input$basemap) 
    outleaf %>% fitBounds(west, south, east, north) 
  ##setView((east+west)/2,(north+south)/2,zoom=7)
  ##fitBounds(west, south, east, north) 
  })
 
  geotop_map <- reactive({ 
    fun_name <- "brickFromOutputSoil3DTensor"
    print(input$variable)
  #  if (is.null(input$variable)) {
  #    args <- variables[[variable_default]][[fun_name]]
  #  } else {
      args <- variables[[input$variable]][[fun_name]]
  #  }  
    
    args$wpath <- wpath
    print(input$time)
  
    args$when <- input$time
    args$zlayer.formatter <- "z%04d"
    print(args)
    outvar <<- do.call(what=fun_name,args=args)
    
    ###print(outvar)
    ## https://rstudio.github.io/leaflet/raster.html
    ###
    
   
    # north <- input$map2_bounds$north
    # south <- input$map2_bounds$south
    # east <-  input$map2_bounds$east
    # west <-  input$map2_bounds$west
    
    ##outleaf <- leafletProxy("map2",session) ##outleaf %>% addProviderTiles(input$basemap) 
    outleaf <- leafletProxy("map2") %>% addProviderTiles(input$basemap) 
    north <- input$map2_bounds$north
    south <- input$map2_bounds$south
    east <-  input$map2_bounds$east
    west <-  input$map2_bounds$west
    outleaf <- outleaf %>% fitBounds(west, south, east, north) 
    legend_name <- "addLegend"
    args_legend <- variables[[input$variable]][[legend_name]]
    ###str(args_legend)
    vargs_legend <<- args_legend
    if (is.null(args_legend$colors) & is.null(args_legend$pal)) {
      args_legend$values <-  values(outvar)
      args_legend$pal <- colorNumeric("OrRd",args_legend$values)
      
      #    args_legend$colors <- rgb(t(col2rgb(palette())) / 255)
      #    args_legend$labels = palette()
    } 
    ##ards_legend$par mandatory???
   ## print(args_legend$pal)
    print("ba")
    args_legend$layerId <- "legend"
    if (args$one.layer==TRUE) { 
      input_layer <- 1 
    } else {
      input_layer <- as.numeric(input$layer)
    }  
    outleaf <- outleaf %>% removeControl(layerId=args_legend$layerId) %>% removeControl(layerId="meteostations")
    outleaf <- outleaf %>% addRasterImage(outvar[[input_layer]],opacity=0.7,colors=args_legend$pal,method="ngb")
    args_legend$map <- outleaf
 
    if (is.null(args_legend$title)) {
      args_legend$title <- paste(input$variable,names(outvar)[input_layer],sep="_")
    }  
    print("HERE!4533@#")
    print(2)
    args_legend <<- args_legend
   ##outleaf <- outleaf %>% do.call(what=addLegend,args=args_legend)
    outleaf <-  do.call(what="addLegend",args=args_legend)
    print("HERE!33@#")
    print(input$meteo)
    if (input$meteo) {
      
  ###    meteov <<- meteo
      outleaf <- outleaf %>% addMarkers(data=meteo,layerId=paste0("meteo_",meteo$MeteoStationCode),icon=icons("icons/meteorology.png",iconWidth=30)) 
    }  else for (itm in paste0("meteo_",meteo$MeteoStationCode)) {
      print(itm)
      outleaf <- outleaf %>% removeMarker(layerId=itm)
    }
    
    
    
    print(input$checkpoints)
    if (input$checkpoints) {
     
      ###    meteov <<- meteo
      outleaf <- outleaf %>% addMarkers(data=checkpoints,layerId=sprintf("check%03d",checkpoints$ID),icon=icons("icons/line-chart.png",iconWidth=30)) 
    } else for (itm in sprintf("check%03d",checkpoints$ID)) {
      print(itm)
      outleaf <- outleaf %>% removeMarker(layerId=itm)
    }
    outleaf 
   
    
  })

  output$dd_prec_temp <- renderDygraph({weather_station_precipitation_temperature()}) 
  output$dd_sw_global_rh <- renderDygraph({weather_station_sw_global_cluod_rh()})
  output$dd_wind <- renderDygraph({weather_station_wind()})
  output$dd_discharge <- renderDygraph({discharge_plot()})
  output$dd_checkpoint <- renderDygraph({checkpoints_timeseries()})
  output$dd_checkpoint_profile <- renderDygraph({checkpoints_timeseries_profile()})
  output$dd_basin <- renderDygraph({basin_plot()})
  
  ## WEATHER  STATION AND CHACKPOINTS POPUP CODE
  marker_click <- reactive ({
    
    ## TO DO
    event <- input$map2_marker_click
    print("a")
    print(event)
    print("b")
    if (is.null(event))
      return()
    
    ##  isolate({
    id <- event$id
    
    
    level <- which(sprintf("check%03d",checkpoints$ID)==id)
    if (length(level)>0) {
      idcheckp <<- sprintf("check%03d",checkpoints$ID) ##paste0("meteo_",meteo$MeteoStationCode[level]) ##GLOBAL ASSIGNEMT
      clicked <- checkpoints[level,] 
      popup <- paste(paste(names(clicked),clicked,sep=" : "),collapse="; ")
      outleaf <-  map <- leafletProxy("map2") %>% clearPopups() %>% addPopups(data=clicked,popup=popup) ###lng=coords0$lng,lat=coords0$lat
    } 
    level <- which(paste0("meteo_",meteo$MeteoStationCode)==id)
    if (length(level)>0) {
      idmeteo <<- paste0("meteo_",meteo$MeteoStationCode[level]) ##GLOBAL ASSIGNEMT
      clicked <- meteo[level,] 
      popup <- paste(paste(names(clicked),clicked,sep=" : "),collapse="; ")
      outleaf <-  map <- leafletProxy("map2") %>% clearPopups() %>% addPopups(data=clicked,popup=popup) ###lng=coords0$lng,lat=coords0$lat
      
      
    }
    outleaf
    
    
    
    
    
    
  })
  
  # ## WEATHER STATION TIME SERIRES DYNAMIC PLOT 
  weather_station_precipitation_temperature <- reactive ({
  
  ## TO DO
  event <- input$map2_marker_click
  print("a")
  print(event)
  print("b")
  if (is.null(event)) {
    id <- meteo$MeteoStationCode[5]
  } else {
    id <- event$id
  } 
  if (!str_detect(id,"meteo")) id <- idmeteo
  print(id)
  level <- which(paste0("meteo_",meteo$MeteoStationCode)==id)
  meteo_clicked <- meteo[level,] ###meteo[] %>% filter(paste0("meteo_",nMeteovar)==id) ## MeteoStationCode==id) ##all_locs[all_locs$location_code0==id,][1,]
  meteodf <- get.geotop.inpts.keyword.value("MeteoFile",wpath=wpath,data.frame=TRUE,
                                            level=level,start_date=input$time0,
                                            end_date=input$time,date_field = nDate,
                                            tz=tz)
  
  
  main <- sprintf("Precipitation Intensity/Air Temperature vs time at %s/%s (%s) Elevation %s m ",   meteo_clicked$MeteoStationName_DE,meteo_clicked$MeteoStationName_IT,
                  meteo_clicked$MeteoStationCode,as.character(meteo_clicked$MeteoStationElevation))
  
  dd <- dygraph(meteodf[,c(nIPrec,nAirTemp)],main=main,ylab=nIPrec) %>% 
    dySeries(nIPrec,label="IPrec",stepPlot=TRUE,fillGraph=TRUE,color="blue") %>%  
    dySeries(nAirTemp, label="AirTemp",axis = 'y2',color="red",) %>%
    dyAxis(label="Air Temperatiore [deg C]",name="y2") %>% 
    dyAxis(label="Precipitation Intensity [mm/hr]",name="y") %>% 
    dyRangeSelector() %>% dyOptions(drawGapEdgePoints=FALSE)
  dd
})
  weather_station_sw_global_cluod_rh <- reactive ({
  
  ## TO DO
  event <- input$map2_marker_click
  print("a")
  print(event)
  print("b")
  if (is.null(event)) {
    id <- meteo$MeteoStationCode[5]
  } else {
    id <- event$id
  }
  if (!str_detect(id,"meteo")) id <- idmeteo
  level <- which(paste0("meteo_",meteo$MeteoStationCode)==id)
  meteo_clicked <- meteo[level,] ###meteo[] %>% filter(paste0("meteo_",nMeteovar)==id) ## MeteoStationCode==id) ##all_locs[all_locs$location_code0==id,][1,]
  meteodf <- get.geotop.inpts.keyword.value("MeteoFile",wpath=wpath,data.frame=TRUE,
                                            level=level,start_date=input$time0,
                                            end_date=input$time,date_field = nDate,
                                            tz=tz)
  
  
  ##  SOLAR RADIATION , CLOUDTRANS AND RH
  meteodf[,nRH] <-  meteodf[,nRH]/100
  
  main <- sprintf("Global Short-Wave Radiation / CloudTrans / RH vs time at %s/%s (%s) Elevation %s m ",   meteo_clicked$MeteoStationName_DE,meteo_clicked$MeteoStationName_IT,
                  meteo_clicked$MeteoStationCode,as.character(meteo_clicked$MeteoStationElevation))
  
  dd <- dygraph(meteodf[,c(nRH,nSWglobal,nCloudTrans)],main=main,ylab=nSWglobal) %>% 
    dySeries(nSWglobal,stepPlot=FALSE,fillGraph=TRUE,color="#feb24c",axis="y",label="SWglobal") %>%  
    dySeries(nRH, axis = 'y2',color="#2b8cbe",label="RH") %>%  dySeries(nCloudTrans, axis = 'y2',color="#756bb1",label="CloudTRans") %>%
    dyAxis(label="RH/CloudTRans [-]",name="y2") %>% 
    dyAxis(label="SW Radiation Intensity [W/m^2]",name="y") %>% 
    dyRangeSelector() %>% dyOptions(drawGapEdgePoints=FALSE)
  
  dd
})
  weather_station_wind <- reactive ({
  
  ## TO DO
  event <- input$map2_marker_click
  print("a")
  print(event)
  print("b")
  if (is.null(event)) {
    id <- meteo$MeteoStationCode[5]
  } else {
    id <- event$id
  }
  if (!str_detect(id,"meteo")) id <- idmeteo
  level <- which(paste0("meteo_",meteo$MeteoStationCode)==id)
  meteo_clicked <- meteo[level,] ###meteo[] %>% filter(paste0("meteo_",nMeteovar)==id) ## MeteoStationCode==id) ##all_locs[all_locs$location_code0==id,][1,]
  meteodf <- get.geotop.inpts.keyword.value("MeteoFile",wpath=wpath,data.frame=TRUE,
                                            level=level,start_date=input$time0,
                                            end_date=input$time,date_field = nDate,
                                            tz=tz)
  
  
  ## PRECIPITATION SOLAR RADIATION , CLOUDTRANS AND RH
  
  main <- sprintf("Wind Speed / Direction  vs time at %s/%s (%s) Elevation %s m ",   meteo_clicked$MeteoStationName_DE,meteo_clicked$MeteoStationName_IT,
                  meteo_clicked$MeteoStationCode,as.character(meteo_clicked$MeteoStationElevation))
  
  
  #x <- (0:360)/360*(2*pi)
  #plot(x,sin(x),type="l")
  #plot(x,asin(sin(x)),type="l")
  #is_north <- (cos(x)>0)
  
  xtheta <- meteodf[,nWindDirection]/360*(2*pi)
  meteodf$WindAngle <- asin(sin(xtheta))/(2*pi)*360
  meteodf$WindNorth <- 90
  meteodf$WindNorth[cos(xtheta)<0] <- -90
  
  dd <- dygraph(meteodf[,c(nWindVelocity,"WindAngle","WindNorth")],main=main,ylab=nWindVelocity) %>% 
    dySeries(nWindVelocity,label="WindSp",axis = 'y',color="blue") %>%  
    dySeries("WindAngle",label="WindDir", axis = 'y2',color="green")  %>% 
    dyShadow("WindNorth",axis="y2",label="North/South") %>%
    dyAxis(label="North/South Angle  [deg Easting]",name="y2") %>% 
    dyAxis(label="speed [m/s]",name="y") %>% 
    dyRangeSelector() %>% dyOptions(drawGapEdgePoints=FALSE)
  
  dd
}) 
  
  
  # ## CHECK POINT  TIME SERIRES DYNAMIC PLOT 
  checkpoints_timeseries <- reactive ({
    
    ## TO DO
    event <- input$map2_marker_click
    print("a")
    print(event)
    print("b")
    if (is.null(event)) {
      id <- meteo$MeteoStationCode[5]
    } else {
      id <- event$id
    } 
    
    if (!(id %in% sprintf("check%03d",checkpoints$ID))) id <<- idcheckp ###########id <- idmeteo !str_detect(id,"meteo")) id <- idmeteo
    print(id)
    level <- which(sprintf("check%03d",checkpoints$ID)==id)
    clicked <- checkpoints[level,] ###meteo[] %>% filter(paste0("meteo_",nMeteovar)==id) ## MeteoStationCode==id) ##all_locs[all_locs$location_code0==id,][1,]
    checkpoint_data <- get.geotop.inpts.keyword.value(checkpoint_key,date_field=date_field_ckp,wpath=wpath,data.frame=TRUE,
                                                      level=level,tz=tz,ContinuousRecovery = 5)
    
    
    main <- sprintf("Variables vs time at %s (%s) Elevation TO CALCULATE m ",   clicked$CoordinatePointName,
                    clicked$CoordinatePointID)
    
    dd <- dygraph(checkpoint_data[,input$checkpoints_variables],main=main,ylab="Variable") %>% 
      dyRangeSelector() 
    dd
  })
  
  # ## CHECK POINT  TIME SERIRES (SOIL PROFILE) DYNAMIC PLOT 
  checkpoints_timeseries_profile <- reactive ({
    
    ## TO DO
    event <- input$map2_marker_click
    print("a")
    print(event)
    print("b")
    if (is.null(event)) {
      id <- meteo$MeteoStationCode[5]
    } else {
      id <- event$id
    } 
    
    if (!(id %in% sprintf("check%03d",checkpoints$ID))) id <<- idcheckp ###########id <- idmeteo !str_detect(id,"meteo")) id <- idmeteo
    print(id)
    level <- which(sprintf("check%03d",checkpoints$ID)==id)
    clicked <- checkpoints[level,] ###meteo[] %>% filter(paste0("meteo_",nMeteovar)==id) ## MeteoStationCode==id) ##all_locs[all_locs$location_code0==id,][1,]
    checkpoint_profile_key <- variables_profile[[input$variablechkp]]$variable
    
    checkpoint_data <- get.geotop.inpts.keyword.value(checkpoint_profile_key,date_field=date_field_ckp,wpath=wpath,data.frame=TRUE,
                                                      level=level,tz=tz,ContinuousRecovery = 5,formatter="%04d",zlayer.formatter="z%04d")
    
    
    main <- sprintf("Variable %s  vs time at %s (%s) Elevation TO CALCULATE m ", input$variablechkp,clicked$CoordinatePointName,
                    clicked$CoordinatePointID)
    
    dd <- dygraph(checkpoint_data,main=main,ylab="Variable") %>% 
      dyRangeSelector() 
    dd
  })
  
  
  
background_map <- reactive({
   north <- input$map2_bounds$north
   south <- input$map2_bounds$south
   east <-  input$map2_bounds$east
   west <-  input$map2_bounds$west
   outleaf <- leafletProxy("map2")


   isolate({
     outleaf <- outleaf %>% fitBounds(west, south, east, north) 
     outleaf <- outleaf %>% addProviderTiles(input$basemap) 

     outleaf
     
     #### GET meteo data 
     #level <- 1:nmeteo
     
     ## set meteo data
     
     #meteo <- get.geotop.inpts.keyword.value("MeteoFile",wpath=wpath,data.frame=TRUE,
                    ###                         level=level,start_date=start,end_date=end,tz=tz)
     
     
     
     
     
     
     
     
   })
   
      
 })
# 
# 

discharge_plot <- reactive({
  discharge_data <- get.geotop.inpts.keyword.value(discharge_keyword,date_field=date_field_discharge,wpath=wpath,data.frame=TRUE,start_date=input$time0,
                                                   end_date=input$time,level=1,tz=tz,formatter = "",ContinuousRecovery = 5) ## only one file
  discharge_data <- discharge_data[,input$discharge_variables]
  dd <- dygraph(discharge_data,ylab="Variables",main="Discharge variable vs Time") %>% 
    dyRangeSelector() %>% dyOptions(drawGapEdgePoints=FALSE)
  
  dd
  
})
basin_plot <- reactive({
  basin_data <- get.geotop.inpts.keyword.value(basin_keyword,date_field=date_field_basin,wpath=wpath,data.frame=TRUE,start_date=input$time0,
                                                   end_date=input$time,level=1,tz=tz,formatter="",ContinuousRecovery = 5) ## only one file
 str(basin_data)
 str(input$basin_variables)
   discharge_data <- basin_data[,input$basin_variables]
  dd <- dygraph(discharge_data,ylab="Variables",main="Basin variable vs Time") %>% 
    dyRangeSelector() %>% dyOptions(drawGapEdgePoints=FALSE)
  
  dd
  
})









observeEvent(input$basemap,{
 geotop_map()
  ##background_map()
  })
observeEvent(input$variable,{geotop_map()})
observeEvent(input$time,{geotop_map()})
observeEvent(input$layer,{geotop_map()})
observeEvent(input$meteo,{geotop_map()})
observeEvent(input$checkpoints,{geotop_map()})
##observeEvent(input$map2_marker_click,{weather_station_click()}) 
###observeEvent(input$map2_marker_click,{checkpoint_click()}) 
observeEvent(input$map2_marker_click,{marker_click()}) 
}


