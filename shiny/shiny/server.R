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
  output$map2 <- renderLeaflet({geotop_map()})
  empty_map <- reactive({
    #isolate(outleaf)
    #isolate(leaflet() %>% addProviderTiles(input$basemap))
    # print("arr")
    # print(basemaps)
    # bbox <- st_bbox(meteo)
    # west <- bbox$xmin
    # south <- bbox$ymin
    # east <- bbox$xmax
    # north <- bbox$ymax
    #outleaf %>% addProviderTiles(input$basemap) %%>fitBounds(west, south, east, north) 
    # 
    outleaf
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
    outleaf <- leaflet() %>% addProviderTiles(input$basemap) 
    legend_name <- "addLegend"
    args_legend <- variables[[input$variable]][[legend_name]]
   
    if (is.null(args_legend$colors) & is.null(args_legend$pal)) {
      args_legend$values <-  values(outvar)
      args_legend$pal <- colorNumeric("OrRd",args_legend$values)
      
      #    args_legend$colors <- rgb(t(col2rgb(palette())) / 255)
      #    args_legend$labels = palette()
    } 
    ##ards_legend$par mandatory???
    print(args_legend$pal)
    args_legend <<- args_legend
    outleaf <- outleaf %>% addRasterImage(outvar[[as.numeric(input$layer)]],opacity=0.7,colors=args_legend$pal,method="ngb")
    args_legend$map <- outleaf
    if (is.null(args_legend$title)) {
      args_legend$title <- paste(input$variable,names(outvar)[as.numeric(input$layer)],sep="_")
    }  
    outleaf <- do.call(what=legend_name,args=args_legend)

    if (input$meteo) {
      
  #    meteo <- get.geotop.points(prefix="MeteoStation",suffix=c("Code","Name_DE","Name_IT","Elevation"),wpath=wpath)
   #   meteo <- meteo %>% st_transform(crs=4326)
      ## meteo_v <- c(st_coordinates(meteo)as.data.frame(meteo))
      outleaf <- outleaf %>% addMarkers(data=meteo)
    }  
    
    
    outleaf  
    
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
     outleaf <- outleaf %>% addRasterImage(outvar[[as.numeric(input$layer)]],opacity=0.7,colors=args_legend$pal,method="ngb") ## ADDED
     outleaf
   })
   
      
 })
# 
# 
observeEvent(input$basemap,{
 ## geotop_map()
  background_map()
  })
observeEvent(input$variable,{geotop_map()})



}
