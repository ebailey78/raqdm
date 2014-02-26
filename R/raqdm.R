user = "ebailey@idem.in.gov"
password = "bolefox64"
query <- list(user = "ebailey@idem.in.gov", pw = "bolefox64", param = "45201", bdate = "20120101", edate = "20121231", state = "18", county = "097")
query <- list(user = "ebailey@idem.in.gov", pw = "bolefox64", param = "45201", bdate = "20120101", edate = "20121231", sate = "18", couny = "097")
query.names <- names(query)
validateURL <- function(params) {
  
  
  
}

validateQuery <- function(...) {
  
  valid.options <- c("user", "pw", "format", "pc", "param", "bdate", "edate", "cbdate", 
                     "cedate", "state", "county", "site", "cbsa", "csa", 
                     "minlat", "maxlat", "minlon", "maxlon", "dur", "frmonly")
  
  query.names <- names(list(...))
  
  vo <- query.names %in% valid.options
  
  # if all parameters weren't found in the list of valid parameters warn the user.
  if(sum(vo) != length(vo)) {
    warning(paste("The following parameters were unrecognized:", paste(query.names[!vo], collapse = ", ")))
  }
  
  # If user isn't provided, check for raqdm.user option value
  if(!("user" %in% query.names)){
    if(is.null(getOption("raqdm.user"))) {
      stop("Must provide a username or set option 'raqdm.user'")
    } else {
      user <- getOption("raqdm.user")
    }
  }

  # If pw isn't provided, check for raqdm.user option value
  if(!("user" %in% query.names)) {
    if(is.null(getOption(raqdm.pw))) {
      stop("Must provide a password or set option 'raqdm.pw'")
    } else {
      password <- getOption("raqdm.pw")
    }
  }
  
  stop.text <- "The following errors were found:\n"
  err <- FALSE
  
  if(!as.logical(sum(c("param", "pc") %in% query.names))) {
    stop.text <- paste(stop.text, "\tparameter class(pc) or a parameter code(param) required\n")
    err <- TRUE
  }
  
  if(!("bdate" %in% query.names & "edate" %in% query.names)) {
    stop.text <- paste(stop.text, "\tbegin date(bdate) and end date(edate) must be provided\n")
    err <- TRUE
  }
  
  bdate <- "1210-01-0"
  bdate <- "20100101"
  nbdate <- try(as.Date(bdate), silent = TRUE)
  if(class(nbdate) == "try-error")
    nbdate <- try(as.Date(bdate, format = "%Y%m%d"))
  if(class(nbdate) == "try-error") {
    stop(nbdate)
  }else{
    bdate <- as.character(nbdate, format = "%Y%m%d")
  }
  
  
  if(nchar)
  
  if(sum(c("minlat", "maxlat", "minlon", "maxlon") %in% query.names) < 4 &
     sum(c("state", "cbsa", "csa") %in% query.names) == 0) {
    stop.text <- paste(stop.text, "\teither bounding box(minlat, maxlat, minlon, maxlon), state(state), core based statistical area(cbsa), or consolidated statistical area(csa) required\n")
    err <- TRUE
  }
  
  if(err) stop(stop.text)
}