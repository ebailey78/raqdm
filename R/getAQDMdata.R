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
  
  x <- httr::content(httr::GET(URL), type = "text")
  
  if(synchronous) {

    stop("Synchronous Requests are currently disabled by EPA")
    
  } else {
    
    request <- list(requestID = x, type = type, format = format)
    class(request) <- "AQDMrequest"
    return(request)
    
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
  
  x <- paste(names(params), params, sep="=", collapse = "&")

  return(x)
  
}