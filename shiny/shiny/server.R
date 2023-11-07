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
 # output$map3 <- renderLeaflet({leaflet() %>% addProviderTiles(input$basemap)})
  output$meteo_station_table <- renderTable({
    if (input$meteo) {
      print(meteo)
      meteo %>% as.data.frame() %>% select(-geometry)
    } else {
      NULL
    }  
    })
  
 ## output$map2 <- renderLeaflet({geotop_map()})
  output$map2 <- renderLeaflet({ 
    outleaf <- leaflet() %>% addProviderTiles(input$basemap) 
    outleaf %>% fitBounds(west, south, east, north) 
  ##setView((east+west)/2,(north+south)/2,zoom=7)
  ##fitBounds(west, south, east, north) 
  })
  # init_map <- reactive({
  #   #isolate(outleaf)
  #   #isolate(leaflet() %>% addProviderTiles(input$basemap))
  #   # print("arr")
  #   # print(basemaps)
  #   # bbox <- st_bbox(meteo)
  #   # west <- bbox$xmin
  #   # south <- bbox$ymin
  #   # east <- bbox$xmax
  #   # north <- bbox$ymax
  #   #outleaf %>% addProviderTiles(input$basemap) %%>fitBounds(west, south, east, north) 
  #   # 
  #   leaflet() %>% addProviderTiles(input$basemap) %>% fitBounds(west, south, east, north) 
  #   })
  
  
  
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
    outleaf <- outleaf %>% removeControl(layerId=args_legend$layerId) %>% removeControl(layerId="meteostations")
    outleaf <- outleaf %>% addRasterImage(outvar[[as.numeric(input$layer)]],opacity=0.7,colors=args_legend$pal,method="ngb")
    args_legend$map <- outleaf
 
    if (is.null(args_legend$title)) {
      args_legend$title <- paste(input$variable,names(outvar)[as.numeric(input$layer)],sep="_")
    }  
    print("HERE!4533@#")

    args_legend <<- args_legend
   ##outleaf <- outleaf %>% do.call(what=addLegend,args=args_legend)
    outleaf <-  do.call(what=addLegend,args=args_legend)
    print("HERE!33@#")
    print(input$meteo)
    if (input$meteo) {
      
  #    meteo <- get.geotop.points(prefix="MeteoStation",suffix=c("Code","Name_DE","Name_IT","Elevation"),wpath=wpath)
   #   meteo <- meteo %>% st_transform(crs=4326)
      ## meteo_v <- c(st_coordinates(meteo)as.data.frame(meteo))
      meteov <<- meteo
      outleaf <- outleaf %>% addMarkers(data=meteo,layerId=paste0("meteo_",meteo$MeteoStationCode))
    }  else for (itm in paste0("meteo_",meteo$MeteoStationCode)) {
      print(itm)
      outleaf <- outleaf %>% removeMarker(layerId=itm)
    }
    
    outleaf 
   
    
  })

   
  weather_station_click <- reactive ({
    
    ## TO DO
    event <- input$map2_marker_click
    print("a")
    print(event)
    print("b")
    if (is.null(event))
      return()
    
    ##  isolate({
    id <- event$id
    level <- which(paste0("meteo_",meteo$MeteoStationCode)==id)
    meteo_clicked <- meteo[level,] ###meteo[] %>% filter(paste0("meteo_",meteo$MeteoStationCode)==id) ## MeteoStationCode==id) ##all_locs[all_locs$location_code0==id,][1,]
    ##coord <- st_coordinates(meteo_clicked)
    
    popup <- paste(paste(names(meteo_clicked),meteo_clicked,sep=" : "),collapse="; ")
    outleaf <-  map <- leafletProxy("map2") %>% clearPopups() %>% addPopups(data=meteo_clicked,popup=popup) ###lng=coords0$lng,lat=coords0$lat
    outleaf
    ###
    ## 202311
    #meteodf <- get.geotop.inpts.keyword.value("MeteoFile",wpath=wpath,data.frame=TRUE,
    #                                        level=level,start_date=input$time0,end_date=input$time,date_field = nDate,tz=tz)
    #time_ <- index(meteodf)
    #meteodf <- as.data.table(meteodf)
    #meteodf$time <- time_
    #meteodf <- meteodf[,c(time,nMeteoVars)]
    #dd <- dygraph(meteodf,main=main,ylab="TO DO") %>% dyRangeSelector()
    
    
#    output$
#    HeaderDateDDMMYYYYhhmmMeteo="Date"
#    HeaderIPrec="N"
#    HeaderWindVelocity="WG"
#    HeaderWindDirection="WR"
#    HeaderRH="LF"
#    HeaderAirTemp="LT"
#    HeaderSWglobal="GS"
#    HeaderCloudSWTransmissivity="CloudTrans"
    ##
    ## Let's change names accortding with Header* keywords? 
    
 #   str(meteodf)
    ###
   
    
    
    ###
    
    
  })
##}

# 
background_map <- reactive({
   north <- input$map2_bounds$north
   south <- input$map2_bounds$south
   east <-  input$map2_bounds$east
   west <-  input$map2_bounds$west
   outleaf <- leafletProxy("map2")


   isolate({
     outleaf <- outleaf %>% fitBounds(west, south, east, north) 
     outleaf <- outleaf %>% addProviderTiles(input$basemap) 
 ##    outleaf <- outleaf %>% addRasterImage(outvar[[as.numeric(input$layer)]],opacity=0.7,colors=args_legend$pal,method="ngb",layerId="raster0") ## ADDED
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
observeEvent(input$basemap,{
 geotop_map()
  ##background_map()
  })
observeEvent(input$variable,{geotop_map()})
observeEvent(input$time,{geotop_map()})
observeEvent(input$layer,{geotop_map()})
observeEvent(input$meteo,{geotop_map()})
observeEvent(input$map2_marker_click,{weather_station_click()})
}
