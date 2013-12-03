importAQDM <- function(x) {
  
  if(missing(x)) x = getParameters()
  
  if(class(x) == "aqdmrequest") {
    
    temp <- tempfile(fileext=".txt")
    download.file(attr(x, "url"), temp)
    if(is.null(attr(x, "params")$format)) {
      y <- read.csv(temp, stringsAsFactors=FALSE, fill=TRUE)    
    } else if(attr(x, "params")$format == "AQS") {
      y <- read.table(temp, sep="|", header = TRUE, comment.char="", stringsAsFactors=FALSE, fill=TRUE)
    } else {
      y <- read.csv(temp, stringsAsFactors=FALSE, fill=TRUE)
    }
    y <- y[seq(nrow(y)-1), ]
    class(y) <- c("AQDM", "data.frame")
    attr(y, "url") <- attr(x, "url")
    attr(y, "params") <- attr(x, "params")
    
  } else if(class(x) == "aqdmparams") {
    
    if(length(params) > 0) {
      
      y <- getForm("https://ofmext.epa.gov/AQDMRS/ws/rawData", .params=x,
                   .opts = list(ssl.verifypeer = FALSE))
      class(y) <- c("AQDM", "data.frame")
      attr(y, "params") <- params
      
    } else {
      
      stop("No query parameters provided.")
      
    }
    
  } else {
    
    stop("Unknown input type")
    
  }
  
  y
  
}