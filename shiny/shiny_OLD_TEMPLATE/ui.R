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
    column(4,  # Adjust the width here
    #sidebarPanel(
    # conditionalPanel(
    #   'input.geotop === "Results"',
      sidebarPanel(width='100%',
        radioButtons("basemap",label="basemap",choices = (basemaps),selected=basemaps[1]),
        
        selectInput("variable", label = "Variable",
                    choices = names(variables), selected = variable_default,width='100%'),
        selectInput("variablechkp", label = "Variable (Soil Profiles in Check Points)",
                    choices = names(variables_profile), selected = variable_profile_default,width='100%'),
        selectInput("layer", label = "layer",
                    choices = 1:5, selected = 1),
        sliderInput("time", label = "time:",
                    min = start, max = end, value = time_default, step = 0.2),
        sliderInput("time0", label = "time0:",
                    min = start, max = end, value = time0_default, step = 0.2),
        checkboxInput("meteo","Weather Stations",value=FALSE),
        checkboxInput("checkpoints","Control Points",value=FALSE),
        selectInput('checkpoints_variables', 'Control Points Variables',nn_checkpoint_vars, multiple=TRUE,selectize=TRUE,selected=nn_checkpoint_vars_default),
        selectInput('discharge_variables', 'Discharge-at-Outlet Variables',nn_discharge_vars, multiple=TRUE,selectize=TRUE,selected=nn_discharge_vars_default),
        selectInput('basin_variables', 'Basin Variables',nn_basin_vars, multiple=TRUE,selectize=TRUE,selected=nn_basin_vars_default)
        ## https://gallery.shinyapps.io/017-select-vs-selectize/
        
      )
      # conditionalPanel(
      #   'input.dataset === "Weather_Forcings"',
      #   helpText("Click the column header to sort a column.")
      # ),
      # conditionalPanel(
      #   'input.dataset === "iris"',
      #   helpText("Display 5 records by default.")
      # )

    )
    #)
    ,
  #  mainPanel(
      #leafletOutput("map2")
  column(8,
      tabsetPanel(
        tabPanel("Results", leafletOutput("map2")),
        tabPanel("Weather_Forcings",
          dygraphOutput("dd_prec_temp"),
          dygraphOutput("dd_sw_global_rh") ,  
          dygraphOutput("dd_wind")                               
         ) ,   
        tabPanel("Control Point Time Series",dygraphOutput("dd_checkpoint")),
        tabPanel("Control Point Time Series (soil profiles)",dygraphOutput("dd_checkpoint_profile")),
        tabPanel("Discharge Time Series",dygraphOutput("dd_discharge")),
        tabPanel("Basin Time Series",dygraphOutput("dd_basin"))
        ##tabPanel("Discharge Time Series",tableOutput("meteo_station_table"))
        
        
        
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
