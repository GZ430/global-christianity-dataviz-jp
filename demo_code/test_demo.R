
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
library(RColorBrewer)
library(lattice)

set.seed(100)

#load data 
countries <- read_csv("https://raw.githubusercontent.com/GZ430/global-christianity-dataviz-jp/main/data/all_countries.csv")
countries |> clean_names()-> countries

all_peoples <- read_csv("https://raw.githubusercontent.com/GZ430/global-christianity-dataviz-jp/main/data/all_peoples_by_country.csv")
all_peoples |> clean_names() -> all_peoples
all_peoples$latitude <- as.numeric(all_peoples$latitude)
all_peoples$longitude <- as.numeric(all_peoples$longitude)
sample_data <- all_peoples[sample(nrow(all_peoples), size = 300), ]

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
            a(href="https://github.com/GZ430/global-christianity-dataviz-jp", 
              "Click here for GitHub repo!")
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
                "Explore by Religion", 
                style = "padding-bottom: 20px"
              ),
              selectInput("religion", 
                          "Select Primary Religion", 
                          unique(all_peoples$primary_religion)),
              actionButton("shuffle", "Click Me to Re-Shuffle"),
              h5("The dots you see on the map represent a random sampling of all 17400 people groups. 
                 The re-shuffle function is coming soon."),
              h5(a(href = "https://joshuaproject.net/global/progress", 
                   "What does each color represent? "))
              
            ),
            mainPanel(
              h5("Click on the dot to see more information about that group."),
              leafletOutput("map_religion", width="800", height="400"),
              h5("The table contains all people groups of the selected religion:"),
              dataTableOutput("table_religion")
              
            )
          )
        ),
        tabPanel(
          "Explore by Continent",
          sidebarLayout(
            sidebarPanel(
              h4(
                "Explore by Continent", 
                style = "padding-bottom: 20px"
              ),
              selectInput("continent", 
                          "Select Continent", 
                          unique(all_peoples$continent)),
              actionButton("shuffle", "Click Me to Re-Shuffle"),
              h5("The dots you see on the map represent a random sampling of all 17400 people groups. 
                 The re-shuffle function is coming soon."),
              h5(a(href = "https://joshuaproject.net/global/progress", 
                   "What does each color represent? "))
            ),
            mainPanel(
              h5("Click on the dot to see more information about that group."),
              leafletOutput("map_continent", width="800", height="400"),
              h5("The table contains all people groups of the selected continent: "),
              dataTableOutput("table_continent")
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
  pal <- colorFactor(pal = c("#FF2E18", "#FFCF2E", "#FFEA13", "#B4FF1F","#1BBF3E"), 
                     domain = c("1", "2", "3", "4", "5"))
  
  filtered_data_religion <- reactive({sample_data|> 
      filter(primary_religion == input$religion)|>
      select(ctry, peop_name_across_countries, population, jp_scale, longitude, latitude)
  })
          output$map_religion <- renderLeaflet({
            df <- filtered_data_religion()
            leaflet(data = df) |> addTiles()|> 
              setView(lng = 30, lat = 30, zoom = 2)|>
              addCircleMarkers(lng = ~longitude, 
                         lat = ~latitude,
                         color = ~pal(jp_scale),
                         stroke = FALSE,
                         fillOpacity = 0.6,
                         popup = paste("Country:", df$ctry, "<br>",
                                       "People Group Name:", df$peop_name_across_countries, "<br>",
                                       "JP Scale:", df$jp_scale,"<br>",
                                       "Population", df$population
                                       ))|> addLegend(pal=pal, 
                                                      values=df$jp_scale,
                                                      opacity = 0.8, 
                                                      na.label = "Not Available")
        
          })
          
          output$table_religion <- renderDataTable({
            df <- all_peoples|> 
              filter(primary_religion == input$religion)|>
              select(ctry, peop_name_across_countries, population, jp_scale)
              colnames(df) <- c("country", "people group name", "population", "JP Scale")
              print(df)
            })
          
      filtered_data_continent<- reactive({sample_data|> 
              filter(continent == input$continent)|>
              select(ctry, peop_name_across_countries, population, jp_scale, longitude, latitude)
          })
        
      
          output$map_continent <- renderLeaflet({
            df <- filtered_data_continent()
            leaflet(data = df) |> addTiles()|> 
              setView(lng = 30, lat = 30, zoom = 2)|>
              addCircleMarkers(
                         lng = ~longitude, 
                         lat = ~latitude,
                         color = ~pal(jp_scale),
                         stroke = FALSE,
                         fillOpacity = 0.6,
                         popup = paste("Country:", df$ctry, "<br>",
                                       "People Group Name:", df$peop_name_across_countries, "<br>",
                                       "JP Scale:", df$jp_scale,"<br>",
                                       "Population", df$population))|> 
              addLegend(pal=pal, 
                        values=df$jp_scale,
                        opacity = 0.8, 
                        na.label = "Not Available")
            
          })
          
          output$table_continent <- renderDataTable({
              df <- all_peoples|> 
              filter(continent == input$continent)|>
              select(ctry, peop_name_across_countries, population, jp_scale)
          colnames(df) <- c("country", "people group name", "population", "JP Scale")
          print(df)
          })
          
          output$table_1 <- renderDataTable(countries)
          output$table_2 <- renderDataTable(countries)
          
    
}


##########################
######## Run App #########
########################## 
shinyApp(ui = ui, server = server)

