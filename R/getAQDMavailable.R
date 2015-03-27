#'@export
getAQDMavailable <- function() {
  
  x <- httr::content(httr::GET("https://ofmext.epa.gov/AQDMRS/ws/serviceAvailable"))
  return(grepl("^READY", x))
  
}