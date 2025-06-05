library(shiny)
library(bslib)
library(shinydashboard)
library(plotly)
library(DT)
library(readxl)

# Data
data <- read_excel("data/polioData.xlsx")

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
                style = "display: flex; flex-direction: row; gap: 30px",
                selectInput("prov", "Provinces", width = 200, c("Toutes les provinces", unique(data$DPS))),
                selectInput("zs", "Zone de santé", width = 200, c("Toutes les zones", unique(data$ZS))),
                selectInput("annee", "Année", width = 200, c("Toutes les années", 2022, 2023, 2024, 2025)),
                selectInput("mois", "Mois", width = 200, c("Tous les mois", month.name)),
              ),
              fluidRow(
                box(title = "Évolution des ventes", width = 12, plotlyOutput("plot1"))
              )
      ),
      tabItem("stats",
              uiOutput("province_cards")
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
      value = nrow(data),
      subtitle = "Total Cas",
      icon = icon("shopping-cart"),
      color = "teal"
    )
  })
  
  output$box2 <- renderValueBox({
    valueBox(
      value = length(unique(data$DPS)),
      subtitle = "Total provinces",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$box3 <- renderValueBox({
    valueBox(
      value = length(unique(data$ZS)),
      subtitle = "Nombre de zone de santé",
      icon = icon("chart-line"),
      color = "navy"
    )
  })
  
  # Evolution des ventes
  # output$plot1 <- renderPlotly({
  #   plot_ly(data, x = ~Date, y = ~Ventes, type = "scatter", mode = "lines+markers",
  #           line = list(color = '#18BC9C')) %>%
  #     layout(title = "Évolution des ventes")
  # })
  
  # Graphique distribution des clients
  output$province_cards <- renderUI({
    provinces <- unique(data$DPS)
    box_list <- lapply(provinces, function(prov) {
      prov_data <- data[data$DPS == prov, ]
      box(
        title = prov,
        width = 4,
        height = 200,
        status = "primary",
        solidHeader = TRUE,
        # valueBox(
        #   value = nrow(prov_data),
        #   subtitle = "Nombre de cas",
        #   icon = icon("virus"),
        #   color = "blue"
        # ),
        # valueBox(
        #   value = length(unique(prov_data$ZS)),
        #   subtitle = "Zones de santé",
        #   icon = icon("hospital"),
        #   color = "teal"
        # )
      )
    })

    fluidRow(box_list)
  })
  
  
  # Table pour afficher les données
  output$datatable <- renderDT({
    datatable(data, options = list(pageLength = 7, autoWidth = TRUE, scrollX = TRUE, scrollY = TRUE, scrollCollapse=TRUE))
  })
}

# Run App
shinyApp(ui = ui, server = server)
