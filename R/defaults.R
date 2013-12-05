getDefault <- function(param) {
  
  dp <- get("default.params", cache)
  return(unlist(dp[param]))
  
}

setDefault <- function(x) {
  
  dp <- get("default.params", cache)
  
  for(i in seq_along(x)) {
    
    if(!is.null(dp[[names(x)[i]]])) dp[names(x)[i]] <- x[[i]]
    
  }
  
  assign("default.params", dp, envir=cache)
  
}

# Performs a list query and returns a dataframe with the result
getValues <- function(name, quals, stop.on.error = TRUE) {
  
  if(missing(quals)) {
    cache.name <- name
  } else {
    cache.name <- paste(name, paste(quals, collapse="."), sep=".")
  }
  cache.name <- paste0(".", cache.name)
  
  if(exists(cache.name, cache)) {
    z <- get(cache.name, cache)
  } else {
    url <- paste0("https://ofmext.epa.gov/AQDMRS/ws/list?name=", name)
    
    if(!missing(quals)) {
      
      qs <- lapply(seq_along(quals), function(i) paste(names(quals)[i], quals[i], sep = "="))
      qs <- paste(qs, collapse = "&")
      url <- paste(url, qs, sep = "&")
      
    }
    
    url <- paste0(url, "&resource=rawData")
    url <- gsub(" ", "%20", url)
    
    x <- getURL(url, .opts = list(ssl.verifypeer = FALSE))
    x <- gsub("'", "", x)
    x <- suppressWarnings(read.table(text = x, sep="\t", colClasses = "character"))
    
    if(ncol(x) == 1) {
      if(stop.on.error) {
        stop(x[1,1])
      } else {
        z <- ""
      }
    } else {
      x[, 2][x[, 2] == "null"] = ""
      z <- x
      colnames(z) <- c("code", "description")
      assign(cache.name, z, envir=cache)
    }
    
  }
  
  return(z)

}