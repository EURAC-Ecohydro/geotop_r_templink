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
library(leaflet)
source("./global.R")



ui <- fluidPage(
  title = "GEOtop Option",
  sidebarLayout(
    #sidebarPanel(
    conditionalPanel(
      'input.geotop === "Results"',
      sidebarPanel(
        radioButtons("basemap",label="basemap",choices = (basemaps),selected=basemaps[1]),
        
        selectInput("variable", label = "Variable",
                    choices = names(variables), selected = variable_default),
        selectInput("layer", label = "layer",
                    choices = 1:5, selected = 1),
        sliderInput("time", label = "time:",
                    min = start, max = end, value = time_default, step = 0.2),
        checkboxInput("meteo","Weather Stations",value=FALSE)
      ),
      conditionalPanel(
        'input.dataset === "mtcars"',
        helpText("Click the column header to sort a column.")
      ),
      conditionalPanel(
        'input.dataset === "iris"',
        helpText("Display 5 records by default.")
      )
    )
    #)
    ,
    mainPanel(
      tabsetPanel(
        id = 'geotop',
        tabPanel("Results", leafletOutput("map2")),
        tabPanel("Weather_Forcings", DT::dataTableOutput("mytable2")),
        tabPanel("iris", DT::dataTableOutput("mytable3"))
      )
    )
  )
)



# 
# # Define UI for application that draws a histogram
# fluidPage(
# # 
# #     # Application title
#      titlePanel("Old Faithful Geyser Data"),
# # 
# #     # Sidebar with a slider input for number of bins
#      sidebarLayout(
#          sidebarPanel(
#             radioButtons("basemap",label="basemap",choices = (basemaps),selected=basemaps[1]),
# 
#             selectInput("variable", label = "Variable",
#             choices = names(variables), selected = "Liquid_Soil_Water_pressure"),
#             selectInput("layer", label = "layer",
#             choices = 1:5, selected = 1),
#             sliderInput("time", label = "time:",
#             min = start, max = end, value = end, step = 0.2),
#             checkboxInput("meteo","Weather Stations",value=FALSE)
#          ),
# #             sliderInput("bins",
# #                         "Number of bins:",
# #                         min = 1,
# #                         max = 50,
# #                         value = 30)
#     
# # 
# #         # Show a plot of the generated distribution
#         mainPanel(
#              leafletOutput("map2") ##, width = "100%", height = "100%")
#       )
# #     ))
# # )
# 
# )
#   
#   
# )
# 
# ###############
# ###############
# ###############
# ###############
# ###############
# 
# 
# ## GSODApp
# ##
# ## Author: Emanuele Cordano
# ## Date: 2022 05 06
# library(shiny)
# source("./global.R")
# 
# ## https://r-craft.org/interactive-visualization-of-geospatial-data-with-r-shiny/
# ui <- fluidPage(
#  
#   sideBarPanel(top = 10, left = 10,width=500, draggable = FALSE,
#     radioButtons("basemap",label="basemap",choices = (basemaps),selected=basemaps[1]),
#     
#     selectInput("variable", label = "Variable",
#                 choices = names(variables), selected = variable_default),
#     selectInput("layer", label = "layer",
#                 choices = 1:5, selected = 1),
#     sliderInput("time", label = "time:",
#                 min = start, max = end, value = time_default, step = 0.2),
#     checkboxInput("meteo","Weather Stations",value=FALSE)
#   ),
#   mainPanel(leafletOutput("map2", width = "100%", height = "100%")),
#   #             sliderInput("bins",
#   #                         "Number of bins:",
#   #                         min = 1,
#   #                         max = 50,
#   #                         value = 30)
#   
#   # 
#   
#   
#   checkboxInput("legend", "Show legend", TRUE)
# )

shinyUI(ui)
