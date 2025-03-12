# Cargar librerías necesarias
library(shiny)
library(shinydashboard)
library(dplyr)
library(shinythemes)
library(plotly)

# Crear el dataset
mvp <- tibble::tibble(
  Season = c("2014-15", "2015-16", "2016-17", "2017-18", "2018-19", 
             "2019-20", "2020-21", "2021-22", "2022-23", "2023-24"),
  Player = c("Stephen Curry", "Stephen Curry", "Russell Westbrook", "James Harden", 
             "Giannis Antetokounmpo", "Giannis Antetokounmpo", "Nikola Jokic", 
             "Nikola Jokic", "Joel Embiid", "Joel Embiid"),
  Points = c(1900, 2375, 2558, 2191, 1994, 1857, 1898, 2004, 2183, 2085),
  Assists = c(480, 527, 840, 630, 451, 354, 599, 584, 274, 708),
  Rebounds = c(342, 342, 878, 542, 994, 978, 1058, 990, 984, 952),
  Steals = c(146, 169, 142, 124, 110, 109, 95, 97, 67, 70)
)

# Definir la interfaz de usuario (UI)
ui <- dashboardPage(
  dashboardHeader(title = "MVP de la NBA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Filtros", icon = icon("filter"), startExpanded = TRUE,
               # Filtro de jugadores (múltiple)
               selectInput("players", "Seleccionar Jugadores:", 
                           choices = unique(mvp$Player),
                           multiple = TRUE),
               # Rango de temporadas
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
      # Página de inicio
      tabItem(tabName = "inicio",
              fluidPage(
                h3("Bienvenido a la aplicación de MVP de la NBA"),
                p("Explora las estadísticas de los jugadores que han ganado el MVP desde la temporada 2014-2015 hasta 2023-2024.")
              )),
      # Página de dashboard
      tabItem(tabName = "dashboard",
              fluidPage(
                h3("Comparación de Puntos por Jugadores"),
                plotlyOutput("compare_bar"),
                h3("Comparación de Asistencias por Jugadores"),
                plotlyOutput("compare_assists"),
                h3("Detalles Seleccionados"),
                tableOutput("compare_details")
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
  
  # Crear gráfica de barras comparativa (puntos)
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
  
  # Crear gráfica de barras comparativa (asistencias)
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
  
  # Mostrar tabla con detalles seleccionados
  output$compare_details <- renderTable({
    filtered_data()
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)

