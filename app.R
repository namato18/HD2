library(shiny)
library(htmltools)
library(DT)
library(bslib)
library(thematic)
library(shinydashboard)
library(plotly)

print(getwd())

my_theme <- bs_theme(version = 5,
                     preset = "darkly",
                     bg = "#00000000",
                     fg = "white",
                     primary = "lightslategrey")

# Define UI
ui <- navbarPage(id = "nav",
                 
                 theme = my_theme,
                 
                 tags$head(
                   tags$style(HTML("
                   
                    body {
                      background-image: url('darker.jpg'); /* Specify the path to your background image */
                      background-size: cover; /* Ensure the image covers the entire tab */
                      background-repeat: no-repeat; /* Prevent image from repeating */
                    }
                    
                    
        
                    .navbar-brand {
                      color: white;
                    }
                    
                    .navbar-nav a {
                      color:white;
                    }
                    
                    .dataTables_paginate, .dataTables_info, .dataTables_filter label, .dataTables_length label, .dataTable {
                      color: white !important;
                    }
                    
                    .navbar-nav .nav-link:hover {
                      color: black !important; /* Change this to your desired hover color */
                    }
                    
                    .navbar-nav .nav-link.active {
                      color: black !important; /* Change this to your desired active color */
                    }
                    "))
                 ),
                 
                 title = "Helldivers 2 Democratic Distribution of Knowledge",
                 
                 tabPanel(title = "Overview",
                          class = "overview-tab",
                          
                          fluidRow(
                            dataTableOutput("overview")
                          ),
                          fluidRow(
                            div(style = "border-top: 1px solid white; width: 100%; margin-top: 20px; margin-bottom: 20px;"),
                            div(style = "text-align: center;",
                                textOutput("totalPopulation")
                            )
                          ),
                          fluidRow(
                            div(style = "border-top: 1px solid white; width: 100%; margin-top: 20px; margin-bottom: 20px;"),
                            column(width = 6,
                                   plotOutput("plot_planets")
                            ),
                            column(width = 6,
                                   plotOutput("plot_factions")
                            )
                          ),
                          div(style = "border-top: 1px solid white; width: 100%; margin-top: 20px; margin-bottom: 20px;"),
                          fluidRow(
                            column(width = 3, strong("Total Bullets Fired:"), textOutput('bulletsFired')),
                            column(width = 3, strong("Total Enemies Killed:"), textOutput('enemiesKilled')),
                            column(width = 3, strong("Total Accidental Friendlies:"), textOutput('accidentalFriendlies')),
                            column(width = 3, strong("Total Time Played:"), textOutput('timePlayed'))
                            
                            
                            

                          )
                          
                 ),
                 
                 tabPanel(title = "Planet Info",
                          class = "planet-info-tab",
                          
                          
                          uiOutput("planet_name"),
                          div(style = "border-top: 1px solid white; width: 100%; margin-top: 20px; margin-bottom: 20px;"),
                          strong("Biome Name: "),
                          textOutput("biomeName"),
                          br(),
                          strong("Biome Description: "),
                          textOutput("biomeDescription"),
                          div(style = "border-top: 1px solid white; width: 100%; margin-top: 20px; margin-bottom: 20px;"),
                          selectInput(inputId = "selectPlanet", label = "Select a Planet to Examine", choices = ""),
                          
                          plotlyOutput("planetPlot")
                          
                          
                          
                 ),
                 
                 tabPanel("Map",
                          tags$head(
                            tags$script(src = "https://d3js.org/d3.v6.min.js"),
                            tags$script(HTML('
      document.addEventListener("DOMContentLoaded", function() {
        var svg = d3.select("#circ_map")
          .append("svg")
          .attr("width", 500)
          .attr("height", 500);
        
        // Create the circular outline
        svg.append("circle")
          .attr("cx", 250)
          .attr("cy", 250)
          .attr("r", 200)
          .style("fill", "none")
          .style("stroke", "black")
          .style("stroke-width", 2);
        
        // Create the red dot and make it clickable
        var redDot = svg.append("circle")
          .attr("cx", 250)
          .attr("cy", 250)
          .attr("r", 5)
          .style("fill", "red")
          .style("cursor", "pointer"); // Change cursor to pointer on hover
          
        // Add click event listener to the red dot
        redDot.on("click", function() {
          // Code to execute when red dot is clicked
          console.log("Red dot clicked!");
        });
      });
    '))
                          ),
                          tags$div(id = "circ_map", style = "width: 500px; height: 500px;")
                          
                 )
                 
                 
)


# Define server logic
server <- function(input, output, session) {
  
  source('Funcs.R')
  
  
  overall_summary <- GetAllPlanets() %>%
    unnest(statistics)
  
  
  output$bulletsFired = renderText(paste0(round(sum(overall_summary$bulletsFired) / 1000000000 , 2), " B"))
  output$enemiesKilled = renderText(paste0(round((sum(overall_summary$terminidKills) + sum(overall_summary$automatonKills)) / 1000000000 ,2 ), " B"))
  output$accidentalFriendlies = renderText(paste0(round(sum(overall_summary$friendlies) / 1000000 ,2), " M"))
  output$timePlayed = renderText(paste0(round(sum(overall_summary$timePlayed) / 60 / 60 / 24 / 365, 2), " Years"))
  

  
  updateSelectInput(inputId = "selectPlanet", label = "Select a Planet to Examine", choices = overall_summary$name)
  
  rv <- reactiveValues(df_planets = character(),
                       df_overview = character())
  
  observe({
    planets_overview = GetCampaignInfo()
    df_planets = planets_overview$df
    df_overview = planets_overview$df_overview
    
    output$overview = renderDataTable({
      datatable(df_planets, escape = FALSE, style = "default", selection = 'single') %>%
        formatStyle(
          'currentOwner',
          target = 'row',
          backgroundColor = styleEqual(c("Automaton","Terminids", "Humans"), c("#FF000080","#FFA50080", "black"))
        )
    })
    
    rv$df_planets = df_planets
    rv$df_overview = df_overview
    
    bar_charts = PlanetPopBar(df_planets)
    
    output$plot_planets = renderPlot(bar_charts$plot_planets)
    output$plot_factions = renderPlot(bar_charts$plot_factions)
    
    sum_planets = sum(df_planets$playerCount)
    
    output$totalPopulation = renderText(paste0("Total Players Online Right Now (excluding inactive planets): ",sum_planets))
    
  })
  
  observeEvent(input$overview_rows_selected, {
    dat <- rv$df_overview[input$overview_rows_selected, ]
    print(dat)
    
    updateNavbarPage(session, inputId = "nav" ,selected = "Planet Info")
    
    output$planet_name = renderUI({
      HTML(paste0("<span style = 'display: flex;
                    justify-content: center;
                    align-items: center;
                    height: 100px;
                    font-size: 20px;
                    font-weight: bold;'>",
                  dat$name,
                  "</span>"))
    })
    
    output$biomeName = renderText(paste0(" ",dat$biome_name))
    output$biomeDescription = renderText(paste0(" ",dat$biome_description)) 
    
    planet_index = rv$df_overview$index[rv$df_overview$name == dat$name]
    planet_history = GetPlanetHistory(planet_id = planet_index)
    
    
    p1 = plot_ly(data = planet_history, x = ~created_at) %>%
      add_lines(y = ~player_count, name = "Player Count", hoverinfo = "y", yaxis = "y1", color = I("blue"), line = list(width = 8), opacity = 0.8) %>%
      add_lines(y = ~liberation, name = "Liberation %", hoverinfo = "y", yaxis = "y2", color = I("green"), line = list(width = 8), opacity = 0.8) %>%
      layout(
        title = "Planet Info",
        xaxis = list(title = "Timeline", gridcolor = "white"),
        yaxis = list(title = "Player Count", side = "left", position = 0, gridcolor = "white"),
        yaxis2 = list(title = "Liberation %", overlaying = "y", side = "right", position = 1, range = c(0,100)),
        plot_bgcolor = "rgba(34, 34, 34, 0.8)",
        paper_bgcolor = "rgba(34, 34, 34, 0.0)",
        font = list(color = "white"))
    
    output$planetPlot = renderPlotly(p1)

  })
  
  observeEvent(input$selectPlanet, {
    
    if(input$selectPlanet != ""){
      print(input$selectPlanet)
      planet_index = overall_summary$index[overall_summary$name == input$selectPlanet]
      print(planet_index)
      GetPlanetHistory(planet_index)
      
      planet_history = GetPlanetHistory(planet_id = planet_index)
      
      if(is.null(planet_history)){
        return(NULL)
      }else{
        p1 = plot_ly(data = planet_history, x = ~created_at) %>%
          add_lines(y = ~player_count, name = "Player Count", hoverinfo = "y", yaxis = "y1", color = I("lightblue"), line = list(width = 8), opacity = 0.4) %>%
          add_lines(y = ~liberation, name = "Liberation %", hoverinfo = "y", yaxis = "y2", color = I("lightgreen"), line = list(width = 8), opacity = 0.4) %>%
          layout(
            title = "Planet Info",
            xaxis = list(title = "Timeline", gridcolor = "grey"),
            yaxis = list(title = "Player Count", side = "left", position = 0, gridcolor = "grey"),
            yaxis2 = list(title = "Liberation %", overlaying = "y", side = "right", position = 1, range = c(0,100)),
            plot_bgcolor = "rgba(34, 34, 34, 0.8)",
            paper_bgcolor = "rgba(34, 34, 34, 0.0)",
            font = list(color = "white"))
        
        output$planetPlot = renderPlotly(p1)
      }
      
      

    }

    
  })
  
  
  
  
  
  
}

# Run the application 
thematic_shiny()
shinyApp(ui = ui, server = server)

