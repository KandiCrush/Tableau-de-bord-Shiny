color_cond_min <- function(value, val_1, val_2) {
  if (is.na(value)) return("black")
  if(value <= val_1) {
    return("green")
  } else if(value <= val_2) {
    return("yellow")
  }
  return("red")
}

color_cond_max <- function(value, val_1, val_2) {
  if (is.na(value)) return("black")
  if(value >= val_1) {
    return("green")
  } else if(value >= val_2) {
    return("yellow")
  }
  return("red")
}