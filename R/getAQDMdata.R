#'getAQDMdata
#'
#'Function for requesting monitoring data from U.S. EPA's Air Quality Data Mart.
#'
#'@param \dots named query parameters to be sent to AQDM.
#'@param synchronous Should a synchronous request be sent (See details)
#'
#'@details 
#'\dots should be a set of name/value pairs where the names correspond to the 
#'variables used by the AQDM web service. 
#'
#'If \code{synchronous = TRUE} then a synchronous request is sent to AQDM. This
#'will result in raw data being immediatiately returned to R. EPA is more strict
#'on the size of the request when using the synchonous service, so for larger 
#'data pulls \code{synchonous = FALSE} may be required.
#'
#'When \code{synchonous = FALSE} then an asynchronous request is sent tp AQDM.
#'This will create an object of class \code{AQDMrequest} being returned. This 
#'object can then be used to retrieve the data from the server when it is ready.
#'
#'@return
#'If \code{synchonous = TRUE} a \code{data.frame} is returned with the requested 
#'data. If \code{synchonous = FALSE} an \code{AQDMrequest} object is returned.
#'The \code{AQDMrequest} object can be used with \code{\link{getAQDMrequest}} to
#'retrieve the data once it is ready.
#'
#'@examples
#'\dontrun{
#'  x <- getAQDMdata(user = "me@@email.com", pw = "abc123", param = "44201",
#'                   state = 18, county = 89, bdate = "20140101",
#'                   edate = "20141231")
#'}      
#'@export
getAQDMdata <- function(..., synchronous = FALSE) {
  
  if(synchronous) {
    type = "rawData"
  } else {
    type = "rawDataNotify"
    format <- getOption("raqdmOptions")$format
    if(!is.null(list(...)$format)) {
      format <- list(...)$format
    }
    if(is.null(format)) {
      format = "DMCSV"
    }
  }
  
  URL <- constructURL(constructAQDMQueryString(...), type)
  
  x <- try(httr::content(httr::GET(URL), type = "text"))
  
  if(class(x) != "try-error") {
    
    if(synchronous) {
  
      stop("Synchronous Requests are currently disabled by EPA")
      
    } else {
      
      request <- list(requestID = x, type = type, format = format, url = URL, 
                      time = Sys.time())
      class(request) <- "AQDMrequest"
      return(request)
      
    }
    
  }
  
}

constructURL <- function(queryString, type = "rawData") {
 
  return(paste0(baseURL, type, "?", queryString))
  
}

constructAQDMQueryString <- function(...) {
  
  p <- list(...)
  n <- names(p)
  params <- getOption("raqdmOptions")
  
  for(i in seq_along(p)) {
    if(n[i] %in% validNames) {
      params[[n[i]]] <- p[[i]]      
    }
  }
  
  params <- verifyVariables(params)
  
  x <- paste(names(params), params, sep="=", collapse = "&")

  return(x)
  
}