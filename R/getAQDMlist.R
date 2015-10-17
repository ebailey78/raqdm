#'getAQDMlist
#'
#'Function for request a list of valid values for a given parameter from
#'U.S. EPA's Air Quality Data Mart.
#'
#'@param name The name of the variable for which you want the list of valid values.
#'@param \dots name/value pairs for subsetting the requested data. (See details)
#'
#'@details
#'  If the variable requires other variables when querying for data you must provide
#'  those other variables to \code{getAQDMlist} as well. (e.g., you must provide a \code{state}
#'  code if \code{name = 'county'}.
#'  
#'@return
#'  Returns a \code{data.frame} with 2 columns. The first column contains the valide code, the
#'  second column contains a description of that code.
#'  
#'@examples
#'\dontrun{
#'  #Get a data.frame of counties in Indiana
#'  counties <- getAQDMlist("county", state = "18")
#'  
#'  #Get a list of criteria pollutants
#'  crit <- getAQDMlist("param", pc = "CRITERIA")
#'}
#'@export
getAQDMlist <- function(name, ...) {
  
  qualifiers <- list(...)
  
  if(name %in% validLists) {
    if(name == "county" & !("state" %in% names(qualifiers))) {
      stop("You must provide a state ID to retrieve a list of counties.") 
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
      URL <- paste0(baseURL, "list?name=", name, "&", quals, "&resource=rawData")
      
      f <- file()
      if(class(try(cat(httr::content(httr::GET(URL)), file = f), silent = TRUE)) != "try-error") { 
        x <- read.delim(f, header= FALSE, colClasses = "character")
      } else {
        stop("There was an error connecting to the AQDM service.")  
      }
      
      close(f)
      return(x)

    }
      
  } else {
    stop("Valid names include:", paste(validLists, collapse = ", "))
  }

}

