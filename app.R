source("helpers.R")

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(readxl)
library(dplyr)
library(ggplot2)
library(DT)
library(bslib)
library(lubridate)
library(plotly)

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

# df <- df[1:2500, ]

# UI --------------------------------------------------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "PLST"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "accueil", icon = icon("home")),
      menuItem("Statistiques", tabName = "stats", icon = icon("chart-bar")),
      menuItem("Evolutions", tabName = "evols", icon = icon("chart-line")),
      menuItem("Répartition", tabName = "graphs", icon = icon("chart-simple")),
      menuItem("Données", tabName = "data", icon = icon("table"))
    )
    
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      .content-wrapper, .right-side {
        overflow-y: hidden;
        height: calc(100vh - 100px); /* 50px ≈ hauteur de l'en-tête */
      }
    "))
    ),
    filtres_ui(df),
    # Accueil ------------------------------------------------------------------------------------------------------------------
    tabItems(
      # Page Accueil (KPIs)
      tabItem(tabName = "accueil",
              
                uiOutput("alert_box"),
              
              fluidRow(
                style = "padding: 0px",
                column(7,
                       style = "padding: 0px",
                       box(
                         title = "Indicateurs",
                         width = 12,
                         # status = "primary",
                         solidHeader = TRUE,
                         height = "calc(100vh - 250px)",
                         plotlyOutput("croise_bar", height = "calc(100vh - 340px)") %>% withSpinner()
                       )
                ),
                column(5,
                       style = "padding: 0px",
                       box(
                         title = "Comparaison des délais moyens",
                         width = 12,
                         solidHeader = TRUE,
                         plotlyOutput("bar_delais", height = "28vh") %>% withSpinner()
                       ),    
                      box(
                        width = 12,
                        plotlyOutput("donut_sexe", height = "28vh") %>% withSpinner()
                      ),
                ),
               
              )
      ),
      # Graphiques trimestriels -------------------------------------------------------------------------------------------
      tabItem(tabName = "stats",
              fluidRow(
                # style = "overflow-y: hidden; height: calc(100vh - 150px);",
                column(
                  12, 
                  style = "overflow-y: scroll; height: calc(100vh - 150px);",
                  uiOutput("province_cards")
                )
              )
      ),
      
      # Evolutions des cas ------------------------------------------------------------------------------------------------
      tabItem(tabName = "evols",
              fluidRow(
                box(
                  title = "Evolution des cas dans le temps", 
                  width = 12,
                  solidHeader = TRUE,
                  height = "calc(100vh - 170px)",
                  plotlyOutput("courbe_temps", height = "calc(100vh - 250px)") %>% withSpinner()
                )
              )
      ),
      # Page Graphiques ---------------------------------------------------------------------------------------------------
      tabItem(tabName = "graphs",
              # Graphiques
              fluidRow(
                style = "padding: 0px; gap: 10px",
                column(7, # à gauche,
                       style = "padding: 0px",
                       box(
                         title = "Répartition des cas", width = 12, solidHeader = TRUE,
                         plotlyOutput("zone_plot", height = "calc(100vh - 250px)") %>% withSpinner(),
                         height = "calc(100vh - 170px)",
                       )
                ),
                column(5, # à droite, les deux petits graphs empilés
                       style = "padding: 0px",
                       box(
                         title = "Répartition par Sexe", width = 12, solidHeader = TRUE,
                         plotlyOutput("sexe_plot", height = "calc(100vh - 580px)") %>% withSpinner(),
                         height = "calc(100vh - 505px)",
                       ),
                       box(
                         title = "Âge des cas", width = 12, solidHeader = TRUE,
                         plotlyOutput("age_hist", height = "calc(100vh - 580px)") %>% withSpinner(),
                         height = "calc(100vh - 505px)",
                       )
                )
              )
      ),
      
      # Page DataTable ----------------------------------------------------------------------------------------------------
      tabItem(tabName = "data",
              style = "overflow-y: scroll; height: calc(100vh - 150px);",
              box(
                title = "Table des Données",
                width = 12,
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
  output$alert_box <- renderUI({
    df <- data_filtre()
    if (nrow(df) == 0) {
      couleur <- "#BDBDBD"
      icon <- icon("info-circle")
      texte <- "Aucune donnée disponible pour la période sélectionnée."
    } else {
      # Tes indicateurs
      moy1 <- round(mean(as.numeric(difftime(df$DateRecAnt, df$Prelevement2, units = "days")), na.rm=TRUE), 1)
      moy2 <- round(mean(as.numeric(difftime(df$DateRecLab, df$DateRecAnt, units = "days")), na.rm=TRUE), 1)
      moy3 <- round(mean(as.numeric(difftime(df$DateRecLab, df$Prelevement2, units = "days")), na.rm=TRUE), 1)
      
      # Seuils
      seuils <- c(3, 2, 5)
      depasse <- c(moy1 > seuils[1], moy2 > seuils[2], moy3 > seuils[3])
      
      if (all(!depasse)) {
        couleur <- "#00C49F"  # Vert
        icon <- icon("check-circle")
        texte <- "Tous les délais moyens sont dans les limites cibles. Aucun dépassement observé."
      } else if (any(depasse)) {
        couleur <- "#FFBB28"  # Orange
        icon <- icon("exclamation-triangle")
        indicateurs <- c("2e prélèvement → Transit", "Transit → INRB", "2e prélèvement → INRB")
        txts <- indicateurs[which(depasse)]
        texte <- paste0("Attention : dépassement de la cible pour : ", paste(txts, collapse = ", "), ".")
        if (sum(depasse) > 1) couleur <- "#F44336"  # Rouge si plusieurs dépassés
      }
    }
    tags$div(
      style = paste(
        "background-color:", couleur, ";",
        "color: #fff;",
        "padding: 18px;",
        "font-size: 16px;",
        "border-radius: 10px;",
        "margin-bottom: 18px;",
        "font-weight: bold;",
        "display: flex; align-items: center;"
      ),
      icon, 
      tags$span(style="margin-left: 14px;", texte)
    )
  })
  
   # KPIs dynamiques
  output$donut_sexe <- renderPlotly({
    df <- data_filtre()
    total <- nrow(df)
    if (total == 0) {
      return(plotly_empty())
    }
    count_F <- sum(df$Sexe == "F", na.rm = TRUE)
    count_M <- sum(df$Sexe == "M", na.rm = TRUE)
    count_autre <- total - count_F - count_M
    labels <- c("Filles", "Garçons", "Non renseigné")
    values <- c(count_F, count_M, count_autre)
    colors <- c('#0088FE', '#00C49F', '#FFBB28')
    
    plot_ly(
      labels = labels,
      values = values,
      type = 'pie',
      hole = 0.6,
      textinfo = 'label+percent',
      insidetextorientation = 'radial',
      marker = list(colors = colors, line = list(color = '#fff', width = 2))
    ) %>%
      layout(
        title = list(text = paste0("Répartition par sexe"), x = 0.5),
        showlegend = TRUE,
        annotations = list(
          list(
            x = 0.5, y = 0.5, text = paste(total, "cas"), showarrow = FALSE, font = list(size = 10)
          )
        ),
        margin = list(l = 30, r = 30, t = 60, b = 30)
      )
  })
  output$bar_delais <- renderPlotly({
    df <- data_filtre()
    moy1 <- round(mean(as.numeric(difftime(df$DateRecAnt, df$Prelevement2, units = "days")), na.rm=TRUE), 1)
    moy2 <- round(mean(as.numeric(difftime(df$DateRecLab, df$DateRecAnt, units = "days")), na.rm=TRUE), 1)
    moy3 <- round(mean(as.numeric(difftime(df$DateRecLab, df$Prelevement2, units = "days")), na.rm=TRUE), 1)
    
    targets <- c(2, 3, 5) # adapte à tes propres seuils
    
    data <- data.frame(
      Libelle = c("2e prélèvement → Transit", "Transit → INRB", "2e prélèvement → INRB"),
      Moyenne = c(moy1, moy2, moy3),
      Cible = targets
    )
    
    plot_ly(
      data,
      x = ~Moyenne,
      y = ~Libelle,
      type = 'bar',
      orientation = 'h',
      name = 'Moyenne',
      marker = list(color = c('#00C49F', '#0088FE', '#FFBB28'))
    ) %>%
      add_trace(
        x = ~Cible,
        y = ~Libelle,
        type = 'violin',
        mode = 'lines+markers',
        name = 'Cible',
        marker = list(symbol = "x", size = 12, color = 'red'),
        line = list(color = 'red')
      ) %>%
      layout(
        barmode = 'group',
        title = "Délais moyens vs objectifs",
        xaxis = list(title = "Nombre de jours"),
        yaxis = list(title = ""),
        legend = list(orientation = "h", x = 0.3, y = -0.5)
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
    print(valeurs)
    colors <- c(
      color_cond_max(valeurs[1], 80, 50),
      color_cond_max(valeurs[2], 80, 50),
      color_cond_max(valeurs[3], 10, 8),
      color_cond_max(valeurs[4], 80, 50)
    )
    # print(colors)
    data.frame(KPI = noms, Valeur = valeurs, Colors = colors)
    
  })
  output$croise_bar <- renderPlotly({
    kpi_bar <- kpi_df()
    plot_ly(
      data = kpi_bar,
      x = ~KPI,
      y = ~Valeur,
      type = 'bar',
      marker = list(color = ~Colors),
      text = ~paste(Valeur, "%"),
      textposition = 'auto',
      hoverinfo = 'text',
      name = 'Indicateurs'
    ) %>%
      layout(
        title = "Indicateurs clés",
        yaxis = list(title = "Valeur (%)"),
        xaxis = list(title = ""),
        margin = list(b = 80),
        showlegend = FALSE
      )
  })
  

  
  # Graphiques statistiques -----------------------------------------------------------------------------------------------

  
    output$province_cards <- renderUI({
    d <- data_filtre()
    provinces <- sort(unique(d$DPS))
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
  
  
  # Evolution des cas -----------------------------------------------------------------------------------------------------
  output$courbe_temps <- renderPlotly({
    cat("Nb lignes data_filtre :", nrow(data_filtre()), "\n")
    groupName <- if (length(unique(data_filtre()$DPS)) == 1) "ZS" else "DPS"
    
    df_tps <- data_filtre() %>%
      group_by(
        !!sym(groupName),
        mois = format(DateDebutParalysie, "%Y-%m")
      ) %>%
      summarise(N = n(), .groups = "drop")
    
    cat("Nb lignes df_tps :", nrow(df_tps), "\n")
    # print(head(df_tps))
    
    if (nrow(df_tps) < 2) {
      plot_ly() %>% layout(title = "Pas assez de données pour afficher la courbe")
    } else {
      df_tps$mois <- factor(df_tps$mois, levels = sort(unique(df_tps$mois)))
      p <- ggplot(df_tps, aes(x = mois, y = N, color = !!sym(groupName), group = !!sym(groupName),
                              text = paste0("Province : ", !!sym(groupName),
                                            "<br>Mois : ", mois,
                                            "<br>Nombre de cas : ", N))) +
        geom_line(size = 0.3) +
        geom_point(size = 0.5) +
        labs(
          x = "Mois",
          y = "Nombre de cas",
          title = "Cas par mois et par province ou par zone de santé",
          color = if (length(unique(data_filtre()$DPS)) == 1) "Zones de santé" else "Provinces"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")
      ggplotly(p, tooltip = "text")
    }
  })
  
  
  # Graphiques -------------------------------------------------------------------------------------------------------
  output$age_hist <- renderPlotly({
    d <- data_filtre()
    if (nrow(d) == 0 || !"Age_Calcule_Annee" %in% colnames(d)) {msg_aucune_donnee(); return(NULL)}
    p <- ggplot(d, aes(x = Age_Calcule_Annee)) +
      geom_histogram(bins = 20, binwidth = 0.5, color = "black", fill = "grey") +
      labs(title = "Distribution de l'âge", x = "Âge (années)", y = "Nombre de cas") +
      theme_bw()
    ggplotly(p)
  })
  output$sexe_plot <- renderPlotly({
    d <- data_filtre()
    if (nrow(d) == 0 || !"Sexe" %in% colnames(d)) {msg_aucune_donnee(); return(NULL)}
    p <- d %>%
      count(Sexe) %>%
      ggplot(aes(x = Sexe, y = n, fill = Sexe, text = paste0("Sexe : ", Sexe, "<br>Nombre : ", n))) +
      geom_bar(stat = "identity") +
      labs(title = "Répartition par Sexe", x = "Sexe", y = "Nombre de cas") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
  })
  output$zone_plot <- renderPlotly({
    groupZone <- if (length(unique(data_filtre()$DPS)) == 1) "ZS" else "DPS"
    val <- if (length(unique(data_filtre()$DPS)) == 1) TRUE else FALSE
    d <- data_filtre()
    if (nrow(d) == 0 || !"ZS" %in% colnames(d)) {msg_aucune_donnee(); return(NULL)}
    p <- d %>%
      count(!!sym(groupZone)) %>%
      ggplot(aes(x = reorder(!!sym(groupZone), n), y = n, text = paste0(if(val) "Zone : " else "Province : ", !!sym(groupZone), "<br>Nombre : ", n))) +
      geom_bar(stat = "identity", fill = "#3182bd") +
      coord_flip() +
      labs(title = if(val) "Cas par Zone de Santé" else "Cas par Province", x = if(val) "Zone de Santé" else "Province", y = "Nombre de cas") +
      theme_minimal()
    ggplotly(p, tooltip = "text")
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