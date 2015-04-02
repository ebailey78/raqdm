#'setAQDMdefaults
#'
#'Convienince function for setting default values for querying the Air Quality
#'Data Mart.
#'
#'@param \dots name/value pairs for querying AQDM (See \link{AQDM Variables})
#'@param save \code{logical} Should the username/password be saved for future sessions?
#'
#'@details
#'\code{raqdm} creates an R option called \code{raqdmOptions}. This holds a list of
#'default values for query parameters passed to \code{\link{getAQDMdata}}. You can
#'use this function to set default values so they don't have to be entered each time
#'you run a query.
#'
#'@return
#'This function returns \code{TRUE} invisibly if there are no problems.
#'
#'@examples
#'  # Sets raqdm to request criteria pollutant data from Indiana by default.
#'  setAQDMdefaults(pc = "CRITERIA", state = "18") 
#'
#'\dontrun{
#'  # You can then use a shortened request to get data for specific county and 
#'  # raqdm will enter the default values automatically.
#'  x <- getAQDMdata(county = "089", bdate = "20130101", edate = "20130228")
#'}
#'
#'@seealso
#'  \href{http://www.epa.gov/airdata/toc.html}{Query Air Data (QAD) User's Guide}
#'  
#'@export
setAQDMdefaults <- function(..., save = TRUE) {
  
  x <- list(...)
  
  if(length(x) == 1 & length(x[[1]]) > 1 & is.list(x[[1]])) {
    x <- x[[1]]
  }
  
  aqdmDefaults <- getOption("raqdmOptions")
  
  for(i in seq(length(x))) {
    if(names(x)[i] %in% validNames) {
      aqdmDefaults[[names(x)[i]]] <- x[[i]]
    } else {
      warning(names(x)[i], " not a valid AQDM parameter. Skipping...")
    }
  }
  
  aqdmDefaults <- verifyVariables(aqdmDefaults, for.request = FALSE)
  
  options("raqdmOptions" = aqdmDefaults)
  
  if(save) {
    save(aqdmDefaults, file = defaultsPath)
  }
  
  return(invisible(TRUE))
  
}
