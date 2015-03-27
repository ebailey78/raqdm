#'getAQDMrequest
#'
#'
#'@export
getAQDMrequest <- function(request, ...) {
  
  if(!(class(request) == "AQDMrequest")) {
    stop("You must provide an 'AQDMrequest' object.")
  }
  
  URL <- paste0(baseURL, "retrieve?id=", request$requestID)
  
  tf <- tempfile()
  if(class(try(download.file(URL, tf, quiet = TRUE), silent = TRUE)) != "try-error") {
  
    if(request$format == "DMCSV") {
      x <- try(read.csv(tf, ...), silent = FALSE)
    } else if(request$format == "AQS") {
      x <- try(read.delim(tf, sep = "|", fill = TRUE, comment.char = "", ...), silent = FALSE) 
      if(class(x) != "try-error") {
         
      }
    } else if(request$format == "AQCSV") {
      x <- try(read.csv(tf, ...), silent = FALSE)     
    } else {
      stop("Unrecognized request format")
    }
    
    if(class(x) == "try-error") {
      stop("Problem accessing data...")
      return(FALSE)
    } else {
      if(x[nrow(x), 1] == "END OF FILE") {
        x <- x[-nrow(x), ]
      }
      return(x)  
    }
    
  } else {
    warning("Data not available yet. Try again shortly, or wait for an email from aqdmrs@epa.gov.")
    return(invisible(FALSE))
  }
  
}