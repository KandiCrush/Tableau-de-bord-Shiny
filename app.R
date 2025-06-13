source("helpers.R")

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)
library(bslib)

# -- Chargement et nettoyage des données
df <- tryCatch({
  read_excel("data/polioData.xlsx") %>%
    mutate(
      Prelevement2 = as.Date(Prelevement2),
      DateRecAnt = as.Date(DateRecAnt),
      DateRecLab = as.Date(DateRecLab)
    ) %>%
    filter(Prelevement2 >= as.Date("1990-01-01") & Prelevement2 <= Sys.Date(),
           DateRecAnt >= as.Date("1990-01-01") & DateRecAnt <= Sys.Date(),
           DateRecAnt >= Prelevement2)
}, error = function(e) {
  stop("Impossible de charger les données. Vérifie ton fichier.")
})

# Fonction pour générer les filtres
filtres_ui <- function() {
  fluidRow(
    column(3, selectInput("dps", "Provinces", choices = c("Toutes", sort(unique(df$DPS))), selected = "Toutes")),
    column(3, selectInput("zs", "Zone de Santé", choices = c("Toutes", sort(unique(df$ZS))), selected = "Toutes")),
    column(6, dateRangeInput("periode", "Période (2e prélèvement)", 
                             start = min(df$Prelevement2, na.rm = TRUE), 
                             end = max(df$Prelevement2, na.rm = TRUE)))
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
                style = "padding: 0px",
                column(9,
                       style = "padding: 0px",
                       box(
                         title = "Indicateurs",
                         width = 12,
                         status = "primary",
                         solidHeader = TRUE,
                         plotOutput("croise_bar") %>% withSpinner()
                       )
                ),
                column(3,
                       style = "padding: 0px;",
                       valueBoxOutput("moy_ant", width = 12),
                       valueBoxOutput("moy_ant_lab", width = 12),
                       valueBoxOutput("moy_lab", width = 12),
                )
              ),
              
              fluidRow(
                box(
                  title = "Evolution des cas dans le temps", 
                  width = 12, 
                  status = "primary", 
                  solidHeader = TRUE,
                  plotOutput("courbe_temps") %>% withSpinner()
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
                         plotOutput("age_hist", height = "calc(100vh - 250px)") %>% withSpinner(),
                         height = "calc(100vh - 170px)",
                       )
                ),
                column(5, # à droite, les deux petits graphs empilés
                       box(
                         title = "Répartition par Sexe", width = 12, status = "info", solidHeader = TRUE,
                         plotOutput("sexe_plot", height = "calc(100vh - 580px)") %>% withSpinner(),
                         height = "calc(100vh - 505px)",
                       ),
                       box(
                         title = "Répartition par Zone de Santé", width = 12, status = "info", solidHeader = TRUE,
                         plotOutput("zone_plot", height = "calc(100vh - 580px)") %>% withSpinner(),
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
      updateSelectInput(session, "zs", choices = c("Toutes", sort(unique(df$ZS))), selected = "Toutes")
    } else {
      zs_choices <- sort(unique(df$ZS[df$DPS == input$dps]))
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
        !is.na(Prelevement2)
      ) %>%
      filter(
        Prelevement2 >= input$periode[1],
        Prelevement2 <= input$periode[2]
      )
    data
  })
  
  df_delai_annee <- reactive({
    data_filtre() %>%
      mutate(
        annee = format(Prelevement2, "%Y"),
        mois  = format(Prelevement2, "%m"),
        annee_mois = paste0(annee, "-", mois),
        delai = as.numeric(difftime(DateRecAnt, Prelevement2, units = "days"))
      ) %>%
      filter(!is.na(annee), !is.na(mois), !is.na(delai), !is.na(DPS)) %>%
      group_by(DPS, annee, mois, annee_mois) %>%
      summarise(
        moyenne = round(mean(delai, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      mutate(
        couleur = sapply(moyenne, function(val) color_cond_min(val, 2, 3))
      )
  })
  
  
  
  # Tableau de bord -------------------------------------------------------------------------------------------------------
  # KPIs dynamiques
  output$n_cas <- renderValueBox({
    valueBox(nrow(data_filtre()), "Nombre de cas", icon = icon("user-injured"), color = "blue")
  })
  output$age_moyen <- renderValueBox({
    valueBox(round(mean(data_filtre()$Age_Calcule_Annee, na.rm=TRUE),1), "Âge moyen (ans)", icon = icon("child"), color = "blue")
  })
  output$female_pourcent <- renderValueBox({
    femmes <- sum(data_filtre()$Sexe == "F", na.rm=TRUE)
    total <- nrow(data_filtre())
    pourcent <- ifelse(total > 0, round(100 * femmes / total, 1), 0)
    valueBox(paste0(pourcent, " %"), "% Filles", icon = icon("venus"), color = "blue")
  })
  output$male_pourcent <- renderValueBox({
    hommes <- sum(data_filtre()$Sexe == "M", na.rm=TRUE)
    total <- nrow(data_filtre())
    pourcent <- ifelse(total > 0, round(100 * hommes / total, 1), 0)
    valueBox(paste0(pourcent, " %"), "% Garçons", icon = icon("mars"), color = "blue")
  })
  
  output$moy_ant <- renderValueBox({
    delai <- as.numeric(difftime(data_filtre()$DateRecAnt, data_filtre()$Prelevement2, units = "days"))
    moyenne <- round(mean(delai, na.rm = TRUE), 1)
    
    valueBox(moyenne,
             "Moyenne de jours entre le 2e prélèvelement et la récéption au point de transit",
             icon = icon("hourglass-half"),
             color = color_cond_min(moyenne, 2, 3)
    )
  })
  output$moy_ant_lab <- renderValueBox({
    delai <- as.numeric(difftime(data_filtre()$DateRecLab, data_filtre()$DateRecAnt, units = "days"))
    moyenne <- round(mean(delai, na.rm = TRUE), 1)
    
    valueBox(moyenne,
             "Moyenne de jours entre la récéption au point de transit et la récéption à l'INRB",
             icon = icon("hourglass-half"),
             color = color_cond_min(moyenne, 1, 2)
    )
  })
  output$moy_lab <- renderValueBox({
    delai <- as.numeric(difftime(data_filtre()$DateRecLab, data_filtre()$Prelevement2, units = "days"))
    moyenne <- round(mean(delai, na.rm = TRUE), 1)
    
    valueBox(moyenne,
             "Moyenne de jours entre le 2e prélèvelement et la récéption à l'INRB",
             icon = icon("hourglass-half"),
             color = color_cond_min(moyenne, 3, 5)
    )
  })
  
  
  # Calcul des KPIs
  kpi_df <- reactive({
    
    nb_cas <- nrow(data_filtre())
    # nb_echantillons <- sum(data$`# des échantillons`, na.rm=TRUE)
    delai <- as.numeric(difftime(data_filtre()$DateRecAnt, data_filtre()$Prelevement2, units = "days"))
    pourcent_transit_2j <- round(100 * sum(delai <= 2, na.rm = TRUE) / sum(!is.na(delai)), 1)
    
    delai <- as.numeric(difftime(data_filtre()$DateRecLab, data_filtre()$Prelevement2, units = "days"))
    pourcent_inrb_3j <- round(100 * sum(delai <= 2, na.rm = TRUE) / sum(!is.na(delai)), 1)
    
    taux_entero_np <- round(100 * (sum(data_filtre()$FinalcellcultureResult == "3-NPENT", na.rm=TRUE) / nb_cas), 1)
    pourcent_selles_adequates <- round(100 * (sum(data_filtre()$`Echantillons Adequat` == "2", na.rm=TRUE)/ nb_cas), 1)
    
    # Noms et valeurs
    noms <- c(
      "% transit ≤ 2 jours",
      "% INRB ≤ 3 jours",
      "Taux Entero-NP (>= 10%)",
      "% selles adéquates"
    )
    valeurs <- c(
      pourcent_transit_2j, pourcent_inrb_3j, taux_entero_np, pourcent_selles_adequates
    )
    colors <- c(
      color_cond_max(valeurs[2],80,50),
      color_cond_max(valeurs[4],80,50),
      color_cond_max(valeurs[1],80,50),
      color_cond_max(valeurs[3],10,8)
    )
    # print(colors)
    data.frame(KPI = noms, Valeur = valeurs, Colors = colors)
  })
  output$croise_bar <- renderPlot({
    kpi_bar <- kpi_df()
    ggplot(kpi_bar, aes(x = KPI, y = as.numeric(Valeur), fill = KPI)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste(Valeur, " %")), vjust = -0.2, size = 5) +
      scale_fill_manual(values = kpi_bar$Colors) +
      labs(title = "Indicateurs clés", x = "", y = "Valeur") +
      theme_minimal()
  })
  
  # Graphiques trimestriels -----------------------------------------------------------------------------------------------
  output$province_cards <- renderUI({
    d <- data_filtre()
    provinces <- unique(d$DPS)
    if (length(provinces) == 0) return(h4("Aucune donnée à afficher."))
    box_list <- lapply(provinces, function(prov) {
      data_plot <- d[d$DPS == prov, ]
      plot_output_id <- paste0("delay_bar_", gsub(" ", "_", prov))
      box(
        title = prov, width = 12,
        plotOutput(plot_output_id, height = 250)
      )
    })
    do.call(fluidRow, box_list)
  })
  
  # output dynamique pour chaque province
  observe({
    provinces <- unique(df_delai_annee()$DPS)
    for (prov in provinces) {
      local({
        province <- prov
        plot_output_id <- paste0("delay_bar_", gsub(" ", "_", province))
        data_plot <- df_delai_annee() %>% filter(DPS == province)
        output[[plot_output_id]] <- renderPlot({
          ggplot(data_plot, aes(x = annee_mois, y = moyenne, fill = couleur, group = 1)) +
            geom_col() +
            geom_text(aes(label = moyenne), vjust = -0.3) +
            scale_fill_identity() +
            labs(
              title = paste0("Moyenne de jours entre le 2e prélèvement et la récéption au point de transit par an (", province, ")"),
              x = "Année/mois",
              y = "Délai moyen (jours)"
            ) +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
          
        })
        
      })
    }
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
  
  # Graphiques -------------------------------------------------------------------------------------------------------
  output$age_hist <- renderPlot({
    d <- data_filtre()
    if (nrow(d) == 0 || !"Age_Calcule_Annee" %in% colnames(d)) {msg_aucune_donnee(); return()}
    ggplot(d, aes(x = Age_Calcule_Annee)) +
      geom_histogram(bins = 20, binwidth = 0.5, color = "black", fill = "grey") +
      labs(title = "Distribution de l'âge", x = "Âge (années)", y = "Nombre de cas") +
      theme_bw()
  })
  output$sexe_plot <- renderPlot({
    d <- data_filtre()
    if (nrow(d) == 0 || !"Sexe" %in% colnames(d)) {msg_aucune_donnee(); return()}
    d %>%
      count(Sexe) %>%
      ggplot(aes(x = Sexe, y = n, fill = Sexe)) +
      geom_bar(stat = "identity") +
      labs(title = "Répartition par Sexe", x = "Sexe", y = "Nombre de cas") +
      theme_minimal()
  })
  output$zone_plot <- renderPlot({
    d <- data_filtre()
    if (nrow(d) == 0 || !"ZS" %in% colnames(d)) {msg_aucune_donnee(); return()}
    d %>%
      count(ZS) %>%
      ggplot(aes(x = reorder(ZS, n), y = n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Cas par Zone de Santé", x = "Zone de Santé", y = "Nombre de cas") +
      theme_minimal()
  })
  
  # Table interactive -------------------------------------------------------------------------------------------------------
  output$datatable <- renderDT({
    d <- data_filtre()
    if (nrow(d) == 0) return(datatable(data.frame("Aucune donnée" = character()), options = list(pageLength = 5, scrollX = TRUE)))
    datatable(d, filter = 'top', options = list(pageLength = 5, scrollX = TRUE))
  })
  output$downloadData <- downloadHandler(
    filename = function() { "data_polio_filtre.csv" },
    content = function(file) {
      write.csv(data_filtre(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)