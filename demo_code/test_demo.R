
##############################################################
#created as a app demo for the global christianity dataviz project
#Grace Zhao
#July 2022

#Template Reference
#https://developer.microsoft.com/en-us/fluentui#/controls/web
#https://appsilon.github.io/shiny.fluent/articles/shiny-fluent.html
#https://github.com/gpilgrim2670/SwimMap/blob/master/app.R
##############################################################

#load libraries
library(shiny)
library(janitor)
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)
library(ggiraph)
library(leaflet)
library(DT)
library(ggrepel)
library(tidyr)
library(shinythemes)
library(lubridate)
library(maps)
library(png)

#load data 
countries <- read_csv("https://raw.githubusercontent.com/GZ430/global-christianity-dataviz-jp/main/data/all_countries.csv")
countries |> clean_names()-> countries



#change select choices name here:
vars_name <- c(
  "what user sees in the drop down" = "actual name in data sets"
)


##########################
##### User interface #####
##########################
  ui <- fluidPage(
    theme = shinytheme("united"),
    navbarPage(
      ##########
      ## Home ##
      ##########
      "Global Christianity Data Exploration",
      tabPanel(
        "Home",
        sidebarLayout(
          sidebarPanel(
            h4(
              "This is a interactive web app that lets you explore global Christianity Data.", 
              style = "padding-bottom: 20px"
            )
          ),
          mainPanel(
            h4("See our data visualizations: "),
            h4(a(href="https://drive.google.com/drive/folders/1pCIvtPP8g018jGli9OZ8BgywmPsbVxT0?usp=sharing", 
              "Click here for gallary!")),
            h6("______________________________________________________"),
            h5("Our Data Source include: "),
            tags$li(a(
              href="https://joshuaproject.net", 
              "Joshua Project"
            )),
            tags$li(a(
              href="https://www.opendoorsusa.org/christian-persecution/world-watch-list", 
              "World Watch List"
            )),
            tags$li(a(
              href="https://www.pewresearch.org/topic/religion", 
              "Pew Research Center"
            )),
            tags$li(a(
              href="https://www.worldbank.org/en/home", 
              "World Bank"
            )),
            h6("______________________________________________________"),
            h1(""),
            h5("This project is ongoing. Visit our Github page for more info."),
            a(href="https://github.com/GZ430/global-christianity-dataviz-jp", "Click here for the GitHub repo!")
          )
        )
      ),
      
      ##########
      ## Maps ##
      ##########
      navbarMenu(
        "View Maps",
        tabPanel(
          "Explore by Religion",
          sidebarLayout(
            sidebarPanel(
              h4(
                "Add input widgets", 
                style = "padding-bottom: 20px"
              )
            ),
            mainPanel(
              leafletOutput("map_1", width="1000", height="600")
            )
          )
        ),
        tabPanel(
          "Explore by Continent",
          sidebarLayout(
            sidebarPanel(
              h4(
                "Add input widgets", 
                style = "padding-bottom: 20px"
              )
            ),
            mainPanel(
              leafletOutput("map_2", width="1000", height="600")
            )
          )
        )
      ),
      
      ############
      ## Metric ##
      ############
      navbarMenu(
        "View Metrics",
        tabPanel(
          "World Watch List Interactive",
          sidebarLayout(
            sidebarPanel(
              h4(
                "Add input widgets", 
                style = "padding-bottom: 20px"
              )
            ),
            mainPanel(
              plotOutput("scatterplot")
              
            )
          )
        ),
        tabPanel(
            "Christian Persecution Status",
            sidebarLayout(
              sidebarPanel(
                h4(
                  "Add input widgets", 
                  style = "padding-bottom: 20px"
                )
              ),
              mainPanel(
                plotOutput("ggmap")
                
              )
            )
        )
      ),
      #################
      ## View Tables ##
      #################
      navbarMenu(
        "Tables",
        tabPanel(
          "Data 1",
          sidebarLayout(
            sidebarPanel(
              h4(
                "Add input widgets", 
                style = "padding-bottom: 20px"
              )
            ),
            mainPanel(
              dataTableOutput("table_1")
            )
          )
        ),
        tabPanel(
          "Data 2",
          sidebarLayout(
            sidebarPanel(
              h4(
                "Add input widgets", 
                style = "padding-bottom: 20px"
              )
            ),
            mainPanel(
              dataTableOutput("table_2")
            )
          )
        )
      )
    )
  )

  

##########################
#### Server Function #####
########################## 
server <- function(input, output) {
   
          output$map_1 <- renderLeaflet({
            leaflet()|> addTiles()
          })
          output$map_2 <- renderLeaflet({
            leaflet()|> addTiles()
          })
          
          output$table_1 <- renderDataTable(countries)
          output$table_2 <- renderDataTable(countries)
          
    
}


##########################
######## Run App #########
########################## 
shinyApp(ui = ui, server = server)

