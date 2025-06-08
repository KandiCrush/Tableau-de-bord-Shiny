library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)
library(bslib)

df <- read_excel("data/polioData.xlsx")
df$DateDebutParalysie <- as.Date(df$DateDebutParalysie)
tableau_croise <- read_excel("data/polioData.xlsx", sheet = 3)

# ------------------------------------------------------------------------------------------------------------------
df_stat <- tableau_croise
df_stat$Année <- NA
df_stat$Province <- NA

annee_en_cours <- NA

for (i in seq_len(nrow(df_stat))) {
  val <- as.character(df_stat$`Étiquettes de lignes`[i])
  if (grepl("^[0-9]{4}$", val)) {
    annee_en_cours <- val
  } else if (val != "Total général" && val != "") {
    df_stat$Année[i] <- annee_en_cours
    df_stat$Province[i] <- val
  }
}

# Filtre pour ne garder que les lignes de province avec année non-NA
df_stat_clean <- df_stat %>%
  filter(!is.na(Année), !is.na(Province))

# ------------------------------------------------------------------------------------------------------------------

kpis <- c(
  "Nombres des cas", 
  "Nombres des échantillons", 
  "moyen de jours entre le 2ième prélèvement et la réception au point de transit (chef-lieu de la province, <=2 jours)", 
  "moyen de jours entre le 2ième prélèvement et la réception à l'INRB (<=3 jours)",
  "% des échantillons acheminés  au point de transit (chef-lieu de la province) dans un délai <=2 jours (>=80%)", 
  "% des échantillons reçus à l'IRNB dans un délai <=3 jours suivant le 2ième prél. (>=80%)",
  "Taux Entero-NP (>=10)",
  "% de selles adéquates (>=80%)"
)

# Fonction pour générer les filtres
filtres_ui <- function() {
  fluidRow(
    column(3, selectInput("dps", "Provinces", choices = c("Toutes", unique(df$DPS)), selected = "Toutes")),
    column(3, selectInput("zs", "Zone de Santé", choices = c("Toutes", unique(df$ZS)), selected = "Toutes")),
    column(6, dateRangeInput("periode", "Période (début paralysie)", 
                             start = min(df$DateDebutParalysie, na.rm = TRUE), 
                             end = max(df$DateDebutParalysie, na.rm = TRUE)))
  )
}



# UI --------------------------------------------------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "PLTS"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "accueil", icon = icon("home")),
      menuItem("Statistiques", tabName = "stats", icon = icon("chart-bar")),
      menuItem("Graphiques", tabName = "graphs", icon = icon("chart-line")),
      menuItem("Données", tabName = "data", icon = icon("table"))
    )
    
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      .content-wrapper, .right-side {
        overflow-y: auto;
        height: calc(100vh - 50px); /* 50px ≈ hauteur de l'en-tête */
      }
    "))
    ),
    filtres_ui(),
      # Accueil ------------------------------------------------------------------------------------------------------------------
    tabItems(
      # Page Accueil (KPIs)
      tabItem(tabName = "accueil",
              fluidRow(
                valueBoxOutput("n_cas", width = 3),
                valueBoxOutput("age_moyen", width = 3),
                valueBoxOutput("female_pourcent", width = 3),
                valueBoxOutput("male_pourcent", width = 3),
                # valueBoxOutput("delai_moyen")
              ),
              
              fluidRow(
                box(
                  title = "Tableau croisé (bar chart)",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  plotOutput("croise_bar")
                )
              ),
              
              fluidRow(
                box(
                  title = "Evolution des cas dans le temps", 
                  width = 12, 
                  status = "primary", 
                  solidHeader = TRUE,
                  plotOutput("courbe_temps")
                )
              )
      ),
      # Graphiques trimestriels -------------------------------------------------------------------------------------------
      tabItem(tabName = "stats",
              uiOutput("province_cards")
      ),
      # Page Graphiques ---------------------------------------------------------------------------------------------------
      tabItem(tabName = "graphs",
        # Graphiques
        fluidRow(
          column(7, # à gauche, l’histogramme d’âge (large)
                 box(
                   title = "Âge des cas", width = 12, status = "info", solidHeader = TRUE,
                   plotOutput("age_hist", height = "calc(100vh - 250px)"),
                   height = "calc(100vh - 170px)",
                 )
          ),
          column(5, # à droite, les deux petits graphs empilés
                 box(
                   title = "Répartition par Sexe", width = 12, status = "info", solidHeader = TRUE,
                   plotOutput("sexe_plot", height = "calc(100vh - 580px)"),
                   height = "calc(100vh - 505px)",
                 ),
                 box(
                   title = "Répartition par Zone de Santé", width = 12, status = "info", solidHeader = TRUE,
                   plotOutput("zone_plot", height = "calc(100vh - 580px)"),
                   height = "calc(100vh - 505px)",
                 )
          )
        )
      ),
      
      # Page DataTable ----------------------------------------------------------------------------------------------------
      tabItem(tabName = "data",
              box(
                title = "Table des Données",
                width = 12,
                status = "primary",
                solidHeader = TRUE,
                DTOutput("datatable"),
                downloadButton("downloadData", "Télécharger les données filtrées")
              )
      )
    )
  )
)

# Server ------------------------------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Mise à jour dynamique des ZS selon la province
  observeEvent(input$dps, {
    if (input$dps == "Toutes") {
      updateSelectInput(session, "zs", choices = c("Toutes", unique(df$ZS)), selected = "Toutes")
    } else {
      zs_choices <- unique(df$ZS[df$DPS == input$dps])
      updateSelectInput(session, "zs", choices = c("Toutes", zs_choices), selected = "Toutes")
    }
  })
  
  data_filtre <- reactive({
    data <- unique(df)
    
    if (input$zs != "Toutes") {
      data <- data %>% filter(ZS == input$zs)
    }
    if (input$dps != "Toutes") {
      data <- data %>% filter(DPS == input$dps)
    }
    data <- data %>%
      filter(
        DateDebutParalysie >= input$periode[1],
        DateDebutParalysie <= input$periode[2]
      )
    
    data
  })
  
  table_filtre <- reactive({
    data <- table_filtre
    if (input$dps != "Toutes") {
      data <- data %>% filter(`Étiquettes de lignes` == input$dps)
    }
    data <- data %>%
      filter(
        DateDebutParalysie >= input$periode[1],
        DateDebutParalysie <= input$periode[2]
      )
    
    data
  })
  
  # Tableau de bord -------------------------------------------------------------------------------------------------------
  # KPIs dynamiques
  output$n_cas <- renderValueBox({
    valueBox(nrow(data_filtre()), "Nombre de cas", icon = icon("user-injured"), color = "blue")
  })
  output$age_moyen <- renderValueBox({
    valueBox(round(mean(data_filtre()$Age_Calcule_Annee, na.rm=TRUE),1), "Âge moyen (ans)", icon = icon("child"), color = "purple")
  })
  output$female_pourcent <- renderValueBox({
    femmes <- sum(data_filtre()$Sexe == "F", na.rm=TRUE)
    total <- nrow(data_filtre())
    pourcent <- ifelse(total > 0, round(100 * femmes / total, 1), 0)
    valueBox(paste0(pourcent, " %"), "% Filles", icon = icon("venus"), color = "yellow")
  })
  output$male_pourcent <- renderValueBox({
    hommes <- sum(data_filtre()$Sexe == "M", na.rm=TRUE)
    total <- nrow(data_filtre())
    pourcent <- ifelse(total > 0, round(100 * hommes / total, 1), 0)
    valueBox(paste0(pourcent, " %"), "% Garçons", icon = icon("mars"), color = "red")
  })

  
  # Calcul des KPIs
  kpi_df <- reactive({
    data <- tableau_croise %>%
      filter(!`Étiquettes de lignes` %in% c("2022", "2023", "2024", "2025"))
    
    if (input$dps != "Toutes") {
      data <- data %>% filter(`Étiquettes de lignes` == input$dps)
    }
      
    nb_cas <- nrow(data)
    # nb_echantillons <- sum(data$`# des échantillons`, na.rm=TRUE)
    delai_moyen_transit <- round(mean(data$`# moyen de jours entre le 2ième prélèvement et la réception au point de transit (chef-lieu de la province, <=2 jours)`, na.rm=TRUE), 1)
    delai_moyen_inrb <- round(mean(data$`# moyen de jours entre le 2ième prélèvement et la réception à l'INRB (<=3 jours)`, na.rm=TRUE), 1)
    pourcent_transit_2j <- round(100 * mean(data$`% des échantillons acheminés  au point de transit (chef-lieu de la province) dans un délai <=2 jours (>=80%)`, na.rm=TRUE), 1)
    pourcent_inrb_3j <- round(100 * mean(data$`% des échantillons reçus à l'IRNB dans un délai <=3 jours suivant le 2ième prél. (>=80%)`, na.rm=TRUE), 1)
    taux_entero_np <- round(mean(data$`Taux Entero-NP (>=10)`, na.rm=TRUE) / nb_cas, 1)
    pourcent_selles_adequates <- round(100 * mean(data$`%de selles adéquates (>=80%)`, na.rm=TRUE), 1)
    
    # Noms et valeurs
    noms <- c(
      # "Nombre de cas",
      # "Nombre d'échantillons",
      "Délai moyen transit (jours)",
      "Délai moyen INRB (jours)",
      "% transit ≤2j",
      "% INRB ≤3j",
      "Taux Entero-NP (%)",
      "% selles adéquates"
    )
    valeurs <- c(
      delai_moyen_transit, delai_moyen_inrb,
      pourcent_transit_2j, pourcent_inrb_3j, taux_entero_np, pourcent_selles_adequates
    )
    data.frame(KPI = noms, Valeur = valeurs)
  })
  
  # Graphiques trimestriels -----------------------------------------------------------------------------------------------
  output$province_cards <- renderUI({
    provinces <- unique(df_stat_clean$Province)
    annees <- sort(unique(df_stat_clean$Année))
    
    box_list <- lapply(provinces, function(prov) {
      data_plot <- df_stat_clean %>% filter(Province == prov, Année %in% annees)
      
      plot_output_id <- paste0("bar_", gsub(" ", "_", prov))
      
      box(
        title = prov,
        width = 12,
        plotOutput(plot_output_id, height = 200)
      )
    })
    
    do.call(fluidRow, box_list)
  })
  
  # output dynamique pour chaque province
  observe({
    provinces <- unique(df_stat_clean$Province)
    annees <- sort(unique(df_stat_clean$Année))
    
    for (prov in provinces) {
      local({
        province <- prov
        plot_output_id <- paste0("bar_", gsub(" ", "_", province))
        data_plot <- df_stat_clean %>% filter(Province == province, Année %in% annees)
        
        output[[plot_output_id]] <- renderPlot({
          jours <- as.numeric(data_plot$`# moyen de jours entre le 2ième prélèvement et la réception au point de transit (chef-lieu de la province, <=2 jours)`)
          data_plot$Couleur <- ifelse(
            jours < 2, "vert",
            ifelse(jours < 3, "jaune", "rouge")
          )
          ggplot(data_plot, aes(
            x = Année,
            y = jours,
            fill = Couleur
          )) +
            geom_bar(stat = "identity") +
            scale_fill_manual(values = c("rouge" = "red", "jaune" = "yellow", "vert" = "green")) +
            labs(
              title = paste0("Délai moyen au point de transit (", province, ")"),
              x = "Année",
              y = "Nombre de jours"
            ) +
            theme_minimal() +
            theme(legend.position = "none")
        })
        
      })
    }
  })
  
  
  # Graphiques -------------------------------------------------------------------------------------------------------
  output$croise_bar <- renderPlot({
    kpi_bar <- kpi_df()
    ggplot(kpi_bar, aes(x = KPI, y = as.numeric(Valeur))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Indicateurs clés", x = "", y = "Valeur") +
      theme_minimal()
  })
  
  
  output$courbe_temps <- renderPlot({
    req(nrow(data_filtre()) > 0)
    df_tps <- data_filtre() %>%
      group_by(semaine = format(DateDebutParalysie, "%Y-%U")) %>%
      summarise(N = n())
    if (nrow(df_tps) < 2) {
      plot.new()
      text(0.5, 0.5, "Pas assez de données pour afficher la courbe")
    } else {
      ggplot(df_tps, aes(x = semaine, y = N, group = 1)) +
        geom_line() +
        geom_point() +
        labs(x = "Semaine", y = "Nombre de cas", title = "Cas par semaine") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  
  output$sexe_plot <- renderPlot({
    req(nrow(data_filtre()) > 0)
    data_filtre() %>%
      count(Sexe) %>%
      ggplot(aes(x = Sexe, y = n, fill = Sexe)) +
      geom_bar(stat = "identity") +
      labs(title = "Répartition par Sexe", x = "Sexe", y = "Nombre de cas") +
      theme_minimal()
  })
  output$zone_plot <- renderPlot({
    req(nrow(data_filtre()) > 0)
    data_filtre() %>%
      count(ZS) %>%
      ggplot(aes(x = reorder(ZS, n), y = n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Cas par Zone de Santé", x = "Zone de Santé", y = "Nombre de cas") +
      theme_minimal()
  })
  output$age_hist <- renderPlot({
    req(nrow(data_filtre()) > 0)
    ggplot(data_filtre(), aes(x = Age_Calcule_Annee)) +
      geom_histogram(bins = 20) +
      labs(title = "Distribution de l'âge", x = "Âge (années)", y = "Nombre de cas") +
      theme_minimal()
  })
  
  # Table interactive -------------------------------------------------------------------------------------------------------
  output$datatable <- renderDT({
    datatable(data_filtre(), filter = 'top', options = list(pageLength = 5, scrollX = TRUE))
  })
  output$downloadData <- downloadHandler(
    filename = function() { "data_polio_filtre.csv" },
    content = function(file) {
      write.csv(data_filtre(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
