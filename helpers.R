# Fonction pour générer les filtres
filtres_ui <- function(df) {
  fluidRow(
    column(3, selectInput("dps", "Provinces", choices = c("Toutes", sort(unique(df$DPS))), selected = "Toutes")),
    column(3, selectInput("zs", "Zone de Santé", choices = c("Toutes", sort(unique(df$ZS))), selected = "Toutes")),
    column(4, dateRangeInput("periode", "Période (2e prélèvement)", 
                             start = min(df$Prelevement2, na.rm = TRUE), 
                             end = max(df$Prelevement2, na.rm = TRUE))),
    # column(2, selectInput(
    #   "type_periode", "Regrouper par",
    #   choices = c("Date" = "date", "Semaine" = "semaine", "Trimestre" = "trimestre", "Semestre" = "semestre"),
    #   selected = "date"
    # )),
  )
}

# Fonction pour colorer les graphiques conditionnellement
color_cond_min <- function(value, val_1, val_2) {
  if (is.na(value)) return("black")
  if(value <= val_1) {
    return("#00C49F")
  } else if(value <= val_2) {
    return("#FFBB28")
  }
  return("#F44336")
}

color_cond_max <- function(value, val_1, val_2) {
  if (is.na(value)) return("black")
  if(value >= val_1) {
    return("#00C49F")
  } else if(value >= val_2) {
    return("#FFBB28")
  }
  return("#F44336")
}