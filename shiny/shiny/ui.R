# App Shiny - GEOtop Interface
# Author: Emanuele Cordano
# Date: October 2023
# License: GPL-3

library(shiny)
library(leaflet)
source("./global.R")
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(
    title =   HTML('<a href="http://www.geotop.org" target="_blank">
  <img src="https://github.com/ecor/geotop/blob/v3.0/doc/logo/geotop_logo.jpg?raw=true" height="40px" width="40px" alt="GEOtop Logo">
</a>  <a href="https://cran.r-project.org/package=geotopbricks" target="_blank">
  <img src="https://github.com/ecor/geotopbricks/raw/master/inst/sticker/sticker_geotopbricks_v2_.png" height="40px" width="40px" alt="geotopbricks CRAN Logo">
</a> GEOtop Hydrological Model / Watershed Simulation'), 
    titleWidth="600px",
    tags$li(class = "dropdown", 
   #         ## imageOutput('image_logo',width="10px",height="10px")
             tags$img(src = "https://github.com/ecor/geotop/blob/v3.0/doc/logo/geotop_logo.jpg?raw=true",height="40px",width="40px")
    )
  
  ),
  dashboardSidebar(width="400px",
  #  column(4,
     #      sidebarPanel(width=12,
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
     ##     )
   # )
  ),
  dashboardBody(
   # column(8,
           tabsetPanel(
             tabPanel("Results", leafletOutput("map2")),
             tabPanel("Weather_Forcings",
                      dygraphOutput("dd_prec_temp"),
                      dygraphOutput("dd_sw_global_rh"),
                      dygraphOutput("dd_wind")
             ) ,   
             tabPanel("Control Point Time Series",dygraphOutput("dd_checkpoint")),
             tabPanel("Control Point Time Series (soil profiles)",dygraphOutput("dd_checkpoint_profile")),
             tabPanel("Discharge Time Series",dygraphOutput("dd_discharge")),
             tabPanel("Basin Time Series",dygraphOutput("dd_basin"))
           ),
           # Footer
           #tags$footer(
           #   style = "text-align: center; background-color: #f8f9fa; padding: 10px;",
          #   "© 2024 My Shiny App"
           #)
           # Footer
          tags$footer(
          style = "text-align: center; background-color: #f8f9fa; padding: 10px;",
          HTML('
            <p><a href="https://www.rendena100.eu" target="_blank">&copy; 2024 Rendena100.eu</a>. All rights reserved.</p>
            <p>Released under the <a href="https://www.gnu.org/licenses/gpl-3.0.html" target="_blank">GPL-3</a> license.</p>
            <p>Acknowledgments: Insert your acknowledgment text here.</p>
          ')
        )     
   # )
  )
)

shinyUI(ui)