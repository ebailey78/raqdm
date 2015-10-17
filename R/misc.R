validNames <- c("user", "pw", "state", "county", "site", "pc", "param", 
                "format", "cbsa", "csa", "dur", "bdate", "edate", "cbdate",
                "cedate", "minlat", "maxlat", "minlon", "maxlon", "frmonly")
validLists <- c("state", "county", "site", "pc", "param", "format", "cbsa", "csa", "dur")
defaultsPath <- normalizePath(paste0(system.file(package="raqdm"), "/defaults.rda"))
#baseURL <- "https://ofmext.epa.gov/AQDMRS/ws/"
baseURL <- "https://aqs.epa.gov/api/"
aqdm <- new.env()

.onLoad <- function(...) {

  aqdmDefaults = NULL
  load(defaultsPath)
  options("raqdmOptions" = aqdmDefaults)
  
}

.onAttach <- function(...) {

  packageStartupMessage("\nU.S. EPA's Air Quality Data Mart requires free registration. Please visit\n\n",
                          "   http://www.epa.gov/airdata/tas_Data_Mart_Registration.html\n\n",
                          "for information on registering. Once registered use\n\n",
                          "   setAQDMuser()\n\n",
                          "to set your username and password for use with raqdm.")
  
}

verifyVariables <- function(params, for.request = TRUE) {
 
  formatDate <- function(d) {
   
    if("Date" %in% class(d) | "POSIXt" %in% class(d)) {
      op <- format(d, "%Y%m%d")
    } else if(is.character(d)) {
      if(is.na(as.numeric(d))) {
        stop("Did not recognize date format.", call. = FALSE)
      } else if(nchar(d) != 8) {
        stop("If providing dates in character format please use YYYYMMDD.", call. = FALSE)
      } else {
        td <- try(as.Date(d, "%Y%m%d"))
        if(class(td) == "try-error") {
          stop("At least one date cannot be interpreted as a date.", call. = FALSE)
        } else {
          op <- d
        }
      }
    }
    
    return(op)
    
  }
  
  np <- names(params)
  
  if(for.request == TRUE) {
    if(!("user" %in% np & "pw" %in% np)) {
      stop("You must provide a username and password to access this feature.", call. = FALSE)
    }
  
    if(!("pc" %in% np | "param" %in% np)) {
      stop("You must define a parameter of interest with either 'pc' or 'param'.", call. = FALSE)
    }
    
    if(!("bdate" %in% np & "edate" %in% np) & !("cbdate" %in% np & "cedate" %in% np)) {
      stop("You must define both a start date and end date (either sampling or change) with 'bdate', 'edate', 'cbdate' and/or 'cedate'.", call. = FALSE)
    }
  
    geoOK <- FALSE
    
    if("cbsa" %in% np) geoOK = TRUE
    if("csa" %in% np) geoOK = TRUE
    if("state" %in% np) geoOK = TRUE
    if("county" %in% np & "state" %in% np) geoOK = TRUE
    if("site" %in% np & "county" %in% np & "state" %in% np) geoOK = TRUE
    if("minlat" %in% np & "maxlat" %in% np & "minlon" %in% np & "maxlon" %in% np) geoOK = TRUE
    
  
    if(!geoOK) {
      stop("You have not adequately defined your geographic area of interest.", call. = FALSE)
    }
  
  }
  
  for(i in c("bdate", "edate", "cbdate", "cedate")) {
    if(i %in% np) {
      params[[i]] = formatDate(params[[i]])
    }
  }  
  
  geolength <- list(state = 2, county = 3, site = 4, cbsa = 5, csa = 3)
  for(i in names(geolength)) {
    if(i %in% np) {
      params[[i]] <- sprintf(paste0("%0", geolength[[i]], "i"), as.integer(params[[i]]))    
    }
  }
  
  if(!is.null(params$frmonly)) {
    frm <- params$frmonly
    if(frm == TRUE | frm %in% c("y", "Y", "yes", "YES", "Yes", "true", "True", "1")) {
      frm = "y"
    } else {
      frm = "n"
    }
    params$frmonly = frm
  }

  return(params)
  
}

#'@export
print.AQDMrequest <- function(x, ...) {
 
  op <- paste0("AQDM Request #", x$requestID, " - Requested: ", 
               x$time, " in ", x$format, " format")
  print(op)
  
}

readFile <- function(file, type, ...) {
  
  if(type == "DMCSV") {
    x <- try(read.csv(file, ...), silent = FALSE)
  } else if(type == "AQS") {
    x <- try(read.delim(file, sep = "|", fill = TRUE, comment.char = ""), silent = FALSE) 
    if(class(x) != "try-error") {
      colnames(x)[1] <- "Transation.Type"
    }
  } else if(type == "AQCSV") {
    x <- try(read.csv(file, ...), silent = FALSE)     
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
  
}