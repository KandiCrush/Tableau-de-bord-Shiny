library(shiny)
library(bslib)
library(shinydashboard)
library(plotly)
library(DT)

# Data
set.seed(123)
data <- data.frame(
  Date = seq(as.Date("2024-01-01"), as.Date("2024-01-30"), by = "day"),
  Ventes = round(runif(30, 100, 1000)),
  Clients = round(runif(30, 10, 50))
)

View(data)

# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = span("Mon Tableau de Bord", style = "font-weight: bold;")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "home", icon = icon("dashboard")),
      menuItem("Statistiques", tabName = "stats", icon = icon("bar-chart")),
      menuItem("Données", tabName = "table", icon = icon("table"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .small-box { font-size: 18px; }
        .skin-blue .main-header .logo { background-color: #2C3E50; color: #fff; }
      "))
    ),
    tabItems(
      tabItem("home",
              fluidRow(
                valueBoxOutput("box1"),
                valueBoxOutput("box2"),
                valueBoxOutput("box3")
              ),
              fluidRow(
                box(title = "Évolution des ventes", width = 12, plotlyOutput("plot1"))
              )
      ),
      tabItem("stats",
              fluidRow(
                box(title = "Statistiques détaillées", width = 12, plotlyOutput("plot2"))
              )
      ),
      tabItem("table",
              fluidRow(
                box(title = "Tableau de données", width = 12, DTOutput("datatable"))
              )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # KPIs
  output$box1 <- renderValueBox({
    valueBox(
      value = sum(data$Ventes),
      subtitle = "Total Ventes",
      icon = icon("shopping-cart"),
      color = "teal"
    )
  })
  
  output$box2 <- renderValueBox({
    valueBox(
      value = sum(data$Clients),
      subtitle = "Total Clients",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$box3 <- renderValueBox({
    valueBox(
      value = round(mean(data$Ventes), 1),
      subtitle = "Vente Moyenne / Jour",
      icon = icon("chart-line"),
      color = "navy"
    )
  })
  
  # Evolution des ventes
  output$plot1 <- renderPlotly({
    plot_ly(data, x = ~Date, y = ~Ventes, type = "scatter", mode = "lines+markers",
            line = list(color = '#18BC9C')) %>%
      layout(title = "Évolution des ventes")
  })
  
  # Graphique distribution des clients
  output$plot2 <- renderPlotly({
    plot_ly(data, x = ~Clients, type = "histogram", nbinsx = 10,
            marker = list(color = '#2C3E50')) %>%
      layout(title = "Distribution des clients par jour")
  })
  
  # Table pour afficher les données
  output$datatable <- renderDT({
    datatable(data, options = list(pageLength = 10, autoWidth = TRUE))
  })
}

# Run App
shinyApp(ui = ui, server = server)
