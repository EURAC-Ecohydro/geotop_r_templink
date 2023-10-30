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
  
  output$map2 <- renderLeaflet({
    fun_name <- "brickFromOutputSoil3DTensor"
    print(input$variable)
    if (is.null(input$variable)) {
      args <- variables[[variable_default]][[fun_name]]
    } else {
      args <- variables[[input$variable]][[fun_name]]
    }  
    
    args$wpath <- wpath
    print(input$time)
  
    args$when <- input$time
    args$zlayer.formatter <- "z%04d"
    print(args)
    outvar <- do.call(what=fun_name,args=args)
    
    print(outvar)
    ## https://rstudio.github.io/leaflet/raster.html
    ###
    
    
    ###
    
    outleaf <- leaflet() %>% addProviderTiles(input$basemap)
  #  north <- input$map2_bounds$north
  #  south <- input$map2_bounds$south
  #  east <-  input$map2_bounds$east
  #  west <-  input$map2_bounds$west
  #  outleaf <- outleaf %>% fitBounds(west, south, east, north) 
    #  for (it in base_providers) {
    # 
    #    outleaf <- outleaf %>% addProviderTiles(it,group=it) 
    #  }
    #  outleaf <- outleaf %>% addLayersControl(baseGroups = base_providers,
    #    position = "topleft"
    #  )
    
   
    ####
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
    outleaf <- outleaf %>% addRasterImage(outvar[[as.numeric(input$layer)]],opacity=0.7,colors=args_legend$pal,method="ngb")
    args_legend$map <- outleaf
    if (is.null(args_legend$title)) {
      args_legend$title <- paste(input$variable,names(outvar)[as.numeric(input$layer)],sep="_")
    }  
    outleaf <- do.call(what=legend_name,args=args_legend)

    if (input$meteo) {
      
      meteo <- get.geotop.points(prefix="MeteoStation",suffix=c("Code","Name_DE","Name_IT","Elevation"),wpath=wpath)
      meteo <- meteo %>% st_transform(crs=4326)
      ## meteo_v <- c(st_coordinates(meteo)as.data.frame(meteo))
      outleaf <- outleaf %>% addMarkers(data=meteo)
    }  
    
    
    outleaf  
    
    
    ## outleaf <- outleaf 
    # hist(faithful$eruptions, probability = TRUE, breaks = as.numeric(input$n_breaks),
    #      xlab = "Duration (minutes)", main = "Geyser eruption duration")
    # 
    # dens <- density(faithful$eruptions, adjust = input$bw_adjust)
    # lines(dens, col = "blue")
  })
  
   

}
