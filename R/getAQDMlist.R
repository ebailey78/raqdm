#'@export
getAQDMlist <- function(name, ...) {
  
  qualifiers <- list(...)
  
  if(name %in% validNames) {
    if(name == "county" & !("state" %in% names(qualifiers))) {
      stop("You must provide a state ID in qualifiers to retrieve a list of counties.") 
    } else if(name == "site" & (!("state" %in% names(qualifiers)) | !("county" %in% names(qualifiers)))) {
      stop("You must provide a state ID and a county ID to retrieve a list of sites.")
    } else {
      
      # Add any missing 0s to the front of state or county qualifiers
      if("state" %in% names(qualifiers)) {
        qualifiers$state = sprintf("%02i", as.integer(qualifiers$state)) 
      }
      if("county" %in% names(qualifiers)) {
        qualifiers$county = sprintf("%03i", as.integer(qualifiers$county)) 
      }    
      
      quals <- paste(names(qualifiers), qualifiers, sep = "=", collapse = "&")
      URL <- paste0("https://ofmext.epa.gov/AQDMRS/ws/list?name=", name, "&", quals, "&resource=rawData")
      
      f <- file()
      cat(httr::content(httr::GET(URL)), file = f)
      x <- read.delim(f, header= FALSE, colClasses = "character")
      close(f)
      return(x)

    }
      
  } else {
    stop("Valid names include:", paste(validNames, collapse = ", "))
  }

}

