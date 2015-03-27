#'@export
getAQDMrequest <- function(request, ...) {
  
  if(!(class(request) == "AQDMrequest")) {
    stop("You must provide an 'AQDMrequest' object.")
  }
  
  URL <- paste0(baseURL, "retrieve?id=", request$requestID)
  
  tf <- tempfile()
  download.file(URL, tf, quiet = TRUE)
  
  if(request$format == "DMCSV") {
    x <- try(read.csv(tf, ...), silent = FALSE)
  } else if(request$format == "AQS") {
    x <- try(read.table(tf, sep = "|", fill = TRUE, ...), silent = FALSE)  
  } else if(request$format == "AQCSV") {
    x <- try(read.csv(tf, ...), silent = FALSE)     
  } else {
    stop("Unrecognized request format")
  }
  
  if(class(x) == "try-error") {
    warning("Data not available yet.")
    return(FALSE)
  } else {
    return(x)  
  }
  
}