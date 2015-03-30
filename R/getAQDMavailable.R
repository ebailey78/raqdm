#'getAQDMavailable
#'
#'A function to check the availability of EPA's AQDM web service
#'
#'@details
#'Makes use of AQDM's serviceAvailable service to check the status of AQDM. 
#'
#'@return 
#'Returns \code{TRUE} if the service reports READY, otherwise returns \code{FALSE}.
#'@examples
#'if(getAQDMavailable()) print("The service is available!")
#'@export
getAQDMavailable <- function() {
  
  x <- httr::content(httr::GET("https://ofmext.epa.gov/AQDMRS/ws/serviceAvailable"))
  return(grepl("^READY", x))
  
}