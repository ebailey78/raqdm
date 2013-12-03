requestAQDM <- function(params) {
  
  if(missing(params)) params = getParameters()
  
  if(length(params) > 0) {
    
    y <- getForm("https://ofmext.epa.gov/AQDMRS/ws/rawDataNotify", .params=params,
                 .opts = list(ssl.verifypeer = FALSE))
    url <- paste0("https://oasext.epa.gov/AQDM/AQDM_", y, ".txt")
    class(y) <- "aqdmrequest"
    
    attr(y, "url") <- url
    attr(y, "params") <- params
    y  
    
  } else {
    stop("No query parameters provided.")
  }
  
}