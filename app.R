
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
library(ggiraph)
library(magrittr)
library(ggmap)

set.seed(100)

#load data 
all_peoples <- read_csv("https://raw.githubusercontent.com/GZ430/global-christianity-dataviz-jp/main/data/all_peoples_by_country.csv")
all_peoples |> clean_names() -> all_peoples
all_peoples$latitude <- as.numeric(all_peoples$latitude)
all_peoples$longitude <- as.numeric(all_peoples$longitude)
sample_data <- all_peoples[sample(nrow(all_peoples), size = 300), ]

wwl_countries <- read_csv("https://raw.githubusercontent.com/GZ430/global-christianity-dataviz-jp/main/data/countrieswwl.csv") |> 
  clean_names()

#change select choices name here:
vars_name <- c(
  "Government Restriction Index 2019" = "dec19gri",
  "World Watch List Score 2022" = "total_score_wwl_2022",
  "WWL-Private Life" = "private_life",
  "WWL-Family Life" = "family_life",
  "WWL-Community Life" = "community_life",
  "WWL-Church Life" = "church_life",
  "WWL-Violence" = "violence",
  "Percent Urbanized" = "percent_urbanized",
  "Literacy Rate" = "literacy_rate",
  "Percent Christianity" = "percent_christianity",
  "Percent Evangelical" = "percent_evangelical",
  "Workers Needed" = "workers_needed"
  
)

#"Human Development Index" = "human_development_index_hdi",
#"Life expectancy at birth" = "life_expectancy_at_birth",
#"Mean years of schooling" = "mean_years_of_schooling",
#"Gross national income (GNI) per capita" = "gross_national_income_gni_per_capita",

colors_name <- c(
  "Continent" = "continent",
  "Primary Religion" = "religion_primary",
  "10-40 Window" = "x10_40window"
)


world_map <- map_data("world")
WWL_rankings <- read_csv("https://raw.githubusercontent.com/GZ430/global-christianity-dataviz-jp/main/data/wwl_2022_rankings.csv")
WWL_rankings |> clean_names()-> WWL_rankings
map.world_WWL22 <- full_join(WWL_rankings, world_map, by = c("country" = "region")) #try left join later ig
map.world_WWL22 <- map.world_WWL22 |>
  mutate(region = country)

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
    ##########
    ## Maps ##
    ##########
    navbarMenu(
      "View Maps",
      tabPanel(
        "Explore by Religion",
        sidebarLayout(
          sidebarPanel(
            h4("Welcome! This is a web app for you to explore 
               global Christianity data. You can interact with maps, view metrics, or check out our data visualizations in About > View Gallery. 
               Have fun!"),
            
            h4(
              "Explore by Religion", 
              style = "padding-bottom: 20px"
            ),
            selectInput("religion", 
                        "Select Primary Religion", 
                        unique(all_peoples$primary_religion)),
            
            actionButton("shuffle", "Click Me to Re-Shuffle"),
            h5("The dots you see on the map only represent a random sampling from all 17400 people groups. 
                 Re-shuffle function coming soon."),
            h5(a(href = "https://joshuaproject.net/global/progress", 
                 "What does JP Scale represent? "))
            
          ),
          mainPanel(
            h5("Click on the dot to see more information about that group."),
            leafletOutput("map_religion", width="800", height="400"),
            h5("All people groups from the selected religion:"),
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
            h5("The dots you see on the map only represent a random sampling from all 17400 people groups. 
                 Re-shuffle function coming soon."),
            h5(a(href = "https://joshuaproject.net/global/progress", 
                 "What does JP Scale represent? "))
          ),
          mainPanel(
            h5("Click on the dot to see more information about that group."),
            leafletOutput("map_continent", width="800", height="400"),
            h5("All people groups from the selected continent: "),
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
            selectInput("x_var", 
                        label = h3("Select X Value"), 
                        choices = vars_name),
            selectInput("y_var", label = h3("Select Y Value"), 
                        choices = vars_name),
            
            selectInput("color", label = h3("Select Color Value"), 
                        choices = colors_name)
          ),
          mainPanel(
            h4("Higher score means more persecution."),
            h5("Hover over the dot for more information."),
            h5(a(href = "https://joshuaproject.net/global/progress", 
                 "What does JP Scale represent? ")),
            ggiraphOutput("scatterplot")
            
          )
        )
      ),
      
      tabPanel(
        "Christian Persecution Status",
        sidebarLayout(
          sidebarPanel(
            h4(
              "Visualize Persecution Status", 
              style = "padding-bottom: 20px"
            ),
            selectInput(inputId = "type", 
                        label = strong("Select Primary Factor"),
                        choices = c("Private Life" = "private_life", 
                                    "Family Life" = "family_life", 
                                    "Community Life" = "community_life", 
                                    "Church Life" = "church_life", 
                                    "Violence" = "violence", 
                                    "Total Score" = "total_score_wwl_2022"))
          ),
          mainPanel(
            h4("Darker color means more persecution."),
            plotOutput("map")
            
          )
        )
      )
    ),
    navbarMenu(
      "About",
    tabPanel(
      "Learn More",
          h4(
            "This is a interactive web app that lets you explore global Christianity Data.", 
            style = "padding-bottom: 20px"),
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
        ),
    tabPanel(
      a(href="https://drive.google.com/drive/folders/1pCIvtPP8g018jGli9OZ8BgywmPsbVxT0?usp=sharing", 
        "View Gallary")
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
              setView(lng = 10, lat = 20, zoom = 2)|>
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
              colnames(df) <- c("Country", "People Group Name", "Population", "JP Scale")
              print(df)
            })
          
          filtered_data_continent<- reactive({sample_data|> 
              filter(continent == input$continent)|>
              select(ctry, peop_name_across_countries, population, jp_scale, longitude, latitude)
          })
        
      
          output$map_continent <- renderLeaflet({
            df <- filtered_data_continent()
            leaflet(data = df) |> addTiles()|> 
              setView(lng = 10, lat = 20, zoom = 2)|>
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
          colnames(df) <- c("Country", "People Group Name", "Population", "JP Scale")
          print(df)
          })
          
          df <- reactive({wwl_countries})
          
          output$scatterplot <- renderggiraph({
            
            p <- ggplot(data = df(), aes_string(x = input$x_var, 
                                                y = input$y_var,
                                                color = input$color)) +
              geom_point_interactive(aes(tooltip = paste(ctry,"<br>","JP Scale: ", jp_scale_ctry), data_id = ctry), alpha = 0.9) +
              theme_minimal()+
              scale_color_brewer(palette = "Dark2")+
              labs(x = "", y = "")+
              guides(color = guide_legend(title = "Selected Color Mapping"))
          
            p <- p + aes(size = population)+ guides(size = guide_legend(title = "Population"))
          
            ggiraph(print(p))
            
          }) 
          
          output$map <- renderPlot({
           
            if (input$type == "private_life") {
              ggplot(data = map.world_WWL22) +
                geom_map(mapping = aes(long, lat, map_id = region), color = "gray", size = 0.1, show.legend = T, map = map.world_WWL22) +
                geom_polygon(aes(x = long, y = lat, group = group, fill = as.numeric(private_life)))+
                theme_void() +
                theme(legend.position = "bottom") +
                scale_fill_distiller(palette = "OrRd", direction = 1) +
                labs(fill = "Private Life Score Per Country") +
                labs(title = "Private Life Score Per Country")
            } else if (input$type == "family_life") {
              ggplot(data = map.world_WWL22) +
                geom_map(mapping = aes(long, lat, map_id = region), color = "gray", size = 0.1, show.legend = T, map = map.world_WWL22) +
                geom_polygon(aes(x = long, y = lat, group = group, fill = as.numeric(family_life)))+
                theme_void() +
                theme(legend.position = "bottom") +
                scale_fill_distiller(palette = "OrRd", direction = 1) +
                labs(fill = "Family Life Score By Country") +
                labs(title = "Family Life Score By Country")
            } else if (input$type == "community_life") {
              ggplot(data = map.world_WWL22) +
                geom_map(mapping = aes(long, lat, map_id = region), color = "gray", size = 0.1, show.legend = T, map = map.world_WWL22) +
                geom_polygon(aes(x = long, y = lat, group = group, fill = as.numeric(community_life)))+
                theme_void() +
                theme(legend.position = "bottom") +
                scale_fill_distiller(palette = "OrRd", direction = 1) +
                labs(fill = "Community Life Score By Country") +
                labs(title = "Community Life Score By Country")
            }else if (input$type == "church_life") {
              ggplot(data = map.world_WWL22) +
                geom_map(mapping = aes(long, lat, map_id = region), color = "gray", size = 0.1, show.legend = T, map = map.world_WWL22) +
                geom_polygon(aes(x = long, y = lat, group = group, fill = as.numeric(church_life)))+
                theme_void() +
                theme(legend.position = "bottom") +
                scale_fill_distiller(palette = "OrRd", direction = 1) +
                labs(fill = "Church Life Score By Country") +
                labs(title = "Church Life Score By Country")
            } else if (input$type == "violence") {
              ggplot(data = map.world_WWL22) +
                geom_map(mapping = aes(long, lat, map_id = region), color = "gray", size = 0.1, show.legend = T, map = map.world_WWL22) +
                geom_polygon(aes(x = long, y = lat, group = group, fill = as.numeric(violence)))+
                theme_void() +
                theme(legend.position = "bottom") +
                scale_fill_distiller(palette = "OrRd", direction = 1) +
                labs(fill = "Violence Score By Country") +
                labs(title = "Violence Score By Country")
            } else if (input$type == "total_score_wwl_2022") {
              ggplot(data = map.world_WWL22) +
                geom_map(mapping = aes(long, lat, map_id = region), color = "gray", size = 0.1, show.legend = T, map = map.world_WWL22) +
                geom_polygon(aes(x = long, y = lat, group = group, fill = as.numeric(total_score_wwl_2022)))+
                theme_void() +
                theme(legend.position = "bottom") +
                scale_fill_distiller(palette = "OrRd", direction = 1) +
                labs(fill = "Total World Watch List Score") +
                labs(title = "2022 World Watch List Scores: Total")
            }
            
          })
    
}

##########################
######## Run App #########
########################## 
shinyApp(ui = ui, server = server)



