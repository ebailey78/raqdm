cache <- new.env()
buildDefault <- as.list(rep("", 18))
names(buildDefault) <- c("user", "pw", "format", "pc", "param", "bdate", "edate", "minlat",
                         "maxlat", "minlon", "maxlon", "state", "county", "site", "cbsa", 
                         "csa", "dur", "frmonly")
assign("default.params", buildDefault, envir = cache)
rm(buildDefault)

# Returns list of default parameters
getDefaults <- function() {
  
  return(get("default.params", cache))
  
}

# Sets default parameters based on names list 'x'
setDefaults <- function(x) {
  
  dp <- get("default.params", cache)
  
  for(i in seq_along(x)) {
    
    if(!is.null(dp[[names(x)[i]]])) dp[names(x)[i]] <- x[[i]]
    
  }
  
  assign("default.params", dp, envir=cache)
  
  TRUE
  
}

# Resets default parameters. 'x' can be a vector of parameters names that 
# should be reset or if missing all defaults are reset.
clearDefaults <- function(x) {
  
  dp <- get("default.params", cache)
  
  if(missing(x)) {
    for(n in names(dp)) {
      dp[n] <- ""
    }
  } else {
    for(p in x) {
      if(!is.null(dp[[p]])) dp[p] <- "" 
    }
  }
    
  assign("default.params", dp, envir=cache)
  
  TRUE
  
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