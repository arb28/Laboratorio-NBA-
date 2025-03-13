# Cargar librerías necesarias
library(shiny)
library(shinydashboard)
library(dplyr)
library(shinythemes)
library(plotly)

# Crear el dataset corregido con estadísticas ofensivas y defensivas
mvp <- tibble::tibble(
  Season = c("2014-15", "2015-16", "2016-17", "2017-18", "2018-19", 
             "2019-20", "2020-21", "2021-22", "2022-23", "2023-24"),
  Player = c("Stephen Curry", "Stephen Curry", "Russell Westbrook", "James Harden", 
             "Giannis Antetokounmpo", "Giannis Antetokounmpo", "Nikola Jokic", 
             "Nikola Jokic", "Joel Embiid", "Nikola Jokic"), 
  Points = c(1900, 2375, 2558, 2191, 1994, 1857, 1898, 2004, 2183, 2200), 
  Assists = c(480, 527, 840, 630, 451, 354, 599, 584, 274, 720), 
  ThreePointers = c(286, 402, 196, 265, 52, 57, 92, 97, 50, 60), 
  ThreePointPercentage = c(44.3, 45.4, 34.3, 36.7, 25.6, 30.4, 38.8, 33.7, 33.0, 37.1), 
  Blocks = c(16, 15, 31, 50, 110, 66, 48, 63, 112, 70), 
  Steals = c(163, 169, 132, 126, 92, 61, 95, 109, 66, 110), 
  DefensiveRebounds = c(285, 362, 727, 348, 739, 716, 575, 813, 557, 760) 
)

# Definir la interfaz de usuario (UI)
ui <- dashboardPage(
  dashboardHeader(title = "MVP de la NBA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Ofensivas", tabName = "ofensivas", icon = icon("basketball-ball")),
      menuItem("Defensivas", tabName = "defensivas", icon = icon("shield-alt")),
      menuItem("Filtros", icon = icon("filter"), startExpanded = TRUE,
               selectInput("players", "Seleccionar Jugadores:", 
                           choices = unique(mvp$Player),
                           multiple = TRUE),
               sliderInput("season_range", "Rango de Temporadas:",
                           min = as.numeric(substr(min(mvp$Season), 1, 4)),
                           max = as.numeric(substr(max(mvp$Season), 1, 4)),
                           value = c(as.numeric(substr(min(mvp$Season), 1, 4)), 
                                     as.numeric(substr(max(mvp$Season), 1, 4))))
      )
    )
  ),
  dashboardBody(
    tabItems(
      # Pestaña de inicio
      tabItem(tabName = "inicio",
              fluidPage(
                h3("Bienvenido a la aplicación de MVP de la NBA"),
                p("Explora las estadísticas de los jugadores que han ganado el MVP desde la temporada 2014-2015 hasta 2023-2024.")
              )),
      # Pestaña de estadísticas ofensivas
      tabItem(tabName = "ofensivas",
              fluidPage(
                h3("Comparación de Puntos por Jugadores"),
                plotlyOutput("compare_bar"),
                h3("Comparación de Asistencias por Jugadores"),
                plotlyOutput("compare_assists"),
                h3("Comparación de Tiros de Tres por Jugadores"),
                plotlyOutput("compare_threes"),
                h3("Efectividad de Tiros de Tres (3P%)"),
                tableOutput("three_point_effectiveness"),
                h3("Detalles Seleccionados"),
                tableOutput("compare_details")
              )),
      # Pestaña de estadísticas defensivas
      tabItem(tabName = "defensivas",
              fluidPage(
                h3("Comparación de Tapones por Jugadores"),
                plotlyOutput("compare_blocks"),
                h3("Comparación de Robos por Jugadores"),
                plotlyOutput("compare_steals"),
                h3("Comparación de Rebotes Defensivos"), 
                plotlyOutput("compare_rebounds"),
                h3("Detalles Defensivos Combinados"),
                tableOutput("defensive_details")
              ))
    )
  )
)

# Definir el servidor
server <- function(input, output, session) {
  # Filtrar datos según los jugadores seleccionados y el rango de temporadas
  filtered_data <- reactive({
    mvp %>%
      filter(Player %in% input$players, 
             as.numeric(substr(Season, 1, 4)) >= input$season_range[1], 
             as.numeric(substr(Season, 1, 4)) <= input$season_range[2])
  })
  
  # Gráficas y tablas de estadísticas ofensivas
  output$compare_bar <- renderPlotly({
    data <- filtered_data()
    plot_ly(data, 
            x = ~Season, 
            y = ~Points, 
            type = "bar", 
            color = ~Player) %>%
      layout(title = "Comparación de Puntos por Jugadores",
             xaxis = list(title = "Temporada"),
             yaxis = list(title = "Puntos"),
             barmode = "group")
  })
  
  output$compare_assists <- renderPlotly({
    data <- filtered_data()
    plot_ly(data, 
            x = ~Season, 
            y = ~Assists, 
            type = "bar", 
            color = ~Player) %>%
      layout(title = "Comparación de Asistencias por Jugadores",
             xaxis = list(title = "Temporada"),
             yaxis = list(title = "Asistencias"),
             barmode = "group")
  })
  
  output$compare_threes <- renderPlotly({
    data <- filtered_data()
    plot_ly(data, 
            x = ~Season, 
            y = ~ThreePointers, 
            type = "bar", 
            color = ~Player) %>%
      layout(title = "Comparación de Tiros de Tres por Jugadores",
             xaxis = list(title = "Temporada"),
             yaxis = list(title = "Tiros de Tres"),
             barmode = "group")
  })
  
  output$compare_details <- renderTable({
    filtered_data()
  })
  
  output$three_point_effectiveness <- renderTable({
    filtered_data() %>% select(Season, Player, ThreePointPercentage)
  })
  
  # Gráficas y tablas de estadísticas defensivas
  output$compare_blocks <- renderPlotly({
    data <- filtered_data()
    plot_ly(data, 
            x = ~Season, 
            y = ~Blocks, 
            type = "bar", 
            color = ~Player) %>%
      layout(title = "Comparación de Tapones por Jugadores",
             xaxis = list(title = "Temporada"),
             yaxis = list(title = "Tapones"),
             barmode = "group")
  })
  
  output$compare_steals <- renderPlotly({
    data <- filtered_data()
    plot_ly(data, 
            x = ~Season, 
            y = ~Steals, 
            type = "bar", 
            color = ~Player) %>%
      layout(title = "Comparación de Robos por Jugadores",
             xaxis = list(title = "Temporada"),
             yaxis = list(title = "Robos"),
             barmode = "group")
  })
  
  output$compare_rebounds <- renderPlotly({ # Nueva gráfica
    data <- filtered_data()
    plot_ly(data, 
            x = ~Season, 
            y = ~DefensiveRebounds, 
            type = "bar", 
            color = ~Player) %>%
      layout(title = "Comparación de Rebotes Defensivos por Jugadores",
             xaxis = list(title = "Temporada"),
             yaxis = list(title = "Rebotes Defensivos"),
             barmode = "group")
  })
  
  output$defensive_details <- renderTable({
    filtered_data() %>% select(Season, Player, Blocks, Steals, DefensiveRebounds) 
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
