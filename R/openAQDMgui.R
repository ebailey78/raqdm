#'openAQDMgui
#'
#'Open a simple GUI to facilitate requesting data from U.S. EPA's Air Quality 
#'Data Mart
#'
#'@details
#'
#'\code{openAQDMgui} opens a dialog that the user can use to manipulate the variables
#'needed to request data from AQDM. 
#'
#'@return
#'The returned value depends on selections made by the user. 
#'
#'If request type is asynchronous
#'and the "Request Data" button is clicked, an \code{AQDMrequest} object will be returned (See
#'\code{\link{getAQDMrequest}}). 
#'
#'If request type is synchronous and the "Request Data" button is clicked,
#'the requested data.frame will be returned once it is returned from the server. 
#'(currently disabled by EPA).
#'
#'If the "Set Defaults" button in clicked, \code{NULL} will be returned invisibly, and 
#'\code{\link{setAQDMdefaults}} will be run with the selected values.
#'
#'If the "Copy Function" button is clicked, a character vector of length one will be returned 
#'container the text of a function that would make the selected request.
#'
#'If the dialog is canceled in any way, \code{NULL} will be returned invisibly.
#'
#'@examples
#'\dontrun{
#'  x <- openAQDMgui()
#'}
#'
#'@export
openAQDMgui <- function() {
  
##### Stuff to set up variables #####
  
  # List to hold widgets
  wid <- list()
  # List to hold frames
  frames <- list()
  # List to hold tclvalues
  values <- list()
  
  # List of variables in the AQDM environment
  env.variables <- ls(aqdm)

  # Any default variables that have been set
  def.variables <- getOption("raqdmOptions")
  
  comboValues <- function(df) {
   
    x <- paste(df[,1], df[,2], sep = " - ")
    x <- c("", x)
    
    return(x)
    
  }
  
  # Loop through lists and if they haven't already been requested this session
  # request them and add them to the aqdm environment.
  for(l in validLists) {
    if(!(l %in% env.variables) & !(l %in% c("site", "county"))) {
      aqdm[[l]] <- getAQDMlist(l)
    } else if(l %in% c("site", "county")) {
      if(l == "county" & !is.null(def.variables$state)) {
        aqdm[[l]] <- getAQDMlist(l, state = def.variables$state)
      } else if(l == "site" & !is.null(def.variables$state) & !is.null(def.variables$county)) {
        aqdm[[l]] <- getAQDMlist(l, state = def.variables$state, county = def.variables$county)
      } else {
        aqdm[[l]] <- NULL
      }
    }
  }
  
  # Loop through all variables making tcl variables and asign their default value,
  # if present.
  for(i in seq_along(validNames)) {
    
    n <- validNames[i]
    d <- def.variables[[n]]
    
    if(!is.null(d)) {  
      if(n %in% validLists) {
        r <- aqdm[[n]][aqdm[[n]][, 1] == d, ]
        d <- paste(r[, 1], r[, 2], sep = " - ")
      }
    } else {
      d = ""
    }
    
    if(n == "frmonly") {
      if(d == "y") {
        d = TRUE
      } else {
        d = FALSE
      }
    }
    
    if("Date" %in% class(d) | "POSIXt" %in% class(d)) {
      d <- format(d, "%Y%m%d")
    }      
    
    values[[n]] <- try(tcltk::tclVar(d), silent = TRUE)
    if(class(values[[n]]) == "try-error") values[[n]] <- tcltk::tclVar("")
    
  }
  

##### Callbacks to control interaction in the GUI #####

# Function to subset parameter combobox by pc combobox selection
  paramUpdate <- function() {
    
    x <- tcltk::tclvalue(values$pc)
    print(x)
    p <- tcltk::tclvalue(values$param)
    
    if(x != "") {
      x <- strsplit(x, " - ")[[1]][1]
      params <- getAQDMlist("param", pc = x)
    } else {
      params <- aqdm$param
    }
    
    newParams <- paste(params[, 1], params[,2], sep = " - ")
    tcltk::tkconfigure(wid$param, values = newParams)
    
    if(!(p %in% newParams)) {
      tcltk::tclvalue(values$param) <- ""
    }
    
  }

  countyUpdate <- function() {
    
    x <- tcltk::tclvalue(values$state)
    p <- tcltk::tclvalue(values$county)
    
    if(x != "") {
      x <- strsplit(x, " - ")[[1]][1]
      counties <- getAQDMlist("county", state = x)
    } else {
      counties <- c()
    }
    
    newCounties <- paste(counties[, 1], counties[, 2], sep = " - ")
    tcltk::tkconfigure(wid$county, values = newCounties)
    
    if(!(p %in% newCounties)) {
      tcltk::tclvalue(values$county) <- ""
    }
    
    siteUpdate()
    
  }

  siteUpdate <- function() {
    
    x <- tcltk::tclvalue(values$state)
    y <- tcltk::tclvalue(values$county)
    p <- tcltk::tclvalue(values$site)
    
    if(x != "" & y != "") {
      x <- strsplit(x, " - ")[[1]][1]
      y <- strsplit(y, " - ")[[1]][1]
      sites <- getAQDMlist("site", state = x, county = y)
    } else {
      sites <- c()
    }
    
    newSites <- paste(sites[, 1], sites[, 2], sep = " - ")
    tcltk::tkconfigure(wid$site, values = newSites)
    
    if(!(p %in% newSites)) {
      tcltk::tclvalue(values$site) <- ""
    }
    
  }

##### Build the actual GUI ####

  window <- tcltk::tktoplevel()
  tcltk::tkwm.title(window, "rAQDM Data Interface")
  frame <- tcltk::ttkframe(window, padding = 10)
  tcltk::tkpack(frame, expand = TRUE, fill = "both")
  
  authFrame <- tcltk::ttklabelframe(frame, text = "Authentication", padding = 5)

    frame$user <- tcltk::ttkframe(authFrame)
    wid$user <- tcltk::ttkentry(frame$user, width = 30, textvariable = values$user)
    tcltk::tkgrid(tcltk::ttklabel(frame$user, text = "Username:", anchor = "e"), wid$user)
    tcltk::tkpack(frame$user, anchor = "e")
    
    frame$pw <- tcltk::ttkframe(authFrame)
    wid$pw <- tcltk::ttkentry(frame$pw, width = 30, textvariable = values$pw)
    tcltk::tkgrid(tcltk::ttklabel(frame$pw, text = "Password:", anchor = "e"), wid$pw)
    tcltk::tkpack(frame$pw, anchor = "e")
  
    tcltk::tkgrid(authFrame, row = 0, column = 0, sticky = "we", padx = c(0, 5))
  
  
  dateFrame <- tcltk::ttklabelframe(frame, text = "Date Ranges", padding = 5)
  
    frame$sampDate <- tcltk::ttkframe(dateFrame)
    wid$bdate <- tcltk::ttkentry(frame$sampDate, width = 10, textvariable = values$bdate)
    wid$edate <- tcltk::ttkentry(frame$sampDate, width = 10, textvariable = values$edate)
    tcltk::tkgrid(tcltk::ttklabel(frame$sampDate, text = "Sampling Dates:", anchor = "e"), wid$bdate, wid$edate)
    tcltk::tkpack(frame$sampDate, anchor = "e")
    
    frame$changeDate <- tcltk::ttkframe(dateFrame)
    wid$cbdate <- tcltk::ttkentry(frame$changeDate, width = 10, textvariable = values$cbdate)
    wid$cedate <- tcltk::ttkentry(frame$changeDate, width = 10, textvariable = values$cedate)
    tcltk::tkgrid(tcltk::ttklabel(frame$changeDate, text = "Change Dates:", anchor = "e"), wid$cbdate, wid$cedate)
    tcltk::tkpack(frame$changeDate, anchor = "e")
  
    tcltk::tkgrid(dateFrame, row = 0, column = 1, sticky = "we", padx = c(0, 5))

  optFrame <- tcltk::ttklabelframe(frame, text = "Options", padding = 5)

    frame$requestType <- tcltk::ttklabelframe(optFrame, text = "Request Type", padding = 5)
      values$requestType <- tcltk::tclVar("rawDataNotify")
      tcltk::tkpack(tcltk::ttkradiobutton(frame$requestType, variable = values$requestType,
                            text = "rawData (synchronous)", value = "rawData"), 
             anchor = "w")
      tcltk::tkpack(tcltk::ttkradiobutton(frame$requestType, variable = values$requestType,
                            text = "rawDataNotify (asynchronous)", value = "rawDataNotify"),
             anchor = "w")
      tcltk::tkpack(frame$requestType, expand = TRUE, fill = "x", anchor = "n")

    frame$format <- tcltk::ttklabelframe(optFrame, text = "Output Format", padding = 5)
      if(tcltk::tclvalue(values$format) == "") tcltk::tclvalue(values$format) <- "DMCSV"
      sapply(seq(nrow(aqdm$format)), function(i) {
        tcltk::tkpack(tcltk::ttkradiobutton(frame$format, variable = values$format, 
                              text = paste(aqdm$format[i,1], aqdm$format[i, 2], sep = " - "),
                              value = aqdm$format[i, 1]), anchor = "w")
      })
      tcltk::tkpack(frame$format, expand = TRUE, fill = "x", anchor = "n")

    frame$dur <- tcltk::ttkframe(optFrame)
      # This cleans up the dur values so they look nicer in the gui
      v <- comboValues(aqdm[["dur"]])
      v <- gsub(" \\(.*\\)", "", v)
      ip <- grepl("PASSIVE", v)
      v <- gsub("INT.*IVE ", "", v)
      v[ip] <- paste(v[ip], "INTEGRATED PASSIVE")
      wid$dur <- tcltk::ttkcombobox(frame$dur, width = 20, textvariable = values$dur, values = v)
      tcltk::tkgrid(tcltk::ttklabel(frame$dur, text = "Duration:", anchor = "e", width = 8), row = 0, column = 0)
      tcltk::tkgrid(wid$dur, row = 0, column = 1, sticky = "we")
      tcltk::tkpack(frame$dur, anchor = "e", expand = TRUE, fill = "x")
      tcltk::tkgrid.columnconfigure(frame$dur, 0, weight = 0)
      tcltk::tkgrid.columnconfigure(frame$dur, 1, weight = 1)

    frame$frmonly <- tcltk::ttkframe(optFrame)
      wid$frmonly <- tcltk::ttkcheckbutton(frame$frmonly, variable = values$frmonly,
                                    text = "FRM/FEM Only")
      tcltk::tkpack(wid$frmonly, anchor = "w")
      tcltk::tkpack(frame$frmonly, anchor = "n", expand = TRUE, fill = "x")

    tcltk::tkgrid(optFrame, row = 0, column = 2, rowspan = 3, sticky = "news")
  
  parFrame  <- tcltk::ttklabelframe(frame, text = "Parameters", padding = 5)
  
    frame$pc <- tcltk::ttkframe(parFrame)
      wid$pc <- tcltk::ttkcombobox(frame$pc, width = 40, textvariable = values$pc, values = comboValues(aqdm[["pc"]]))
      tcltk::tkgrid(wid$pc, row = 0, column = 1, sticky = "we")
      tcltk::tkgrid(tcltk::ttklabel(frame$pc, text = "Parameter Class:", anchor = "e", width = 18), row = 0, column = 0)
      tcltk::tkpack(frame$pc, anchor = "e", expand = TRUE, fill = "x")
      tcltk::tkgrid.columnconfigure(frame$pc, 0, weight = 0)
      tcltk::tkgrid.columnconfigure(frame$pc, 1, weight = 1)
    
    frame$param <- tcltk::ttkframe(parFrame)
      wid$param <- tcltk::ttkcombobox(frame$param, width = 40, textvariable = values$param, values = comboValues(aqdm[["param"]]))
      tcltk::tkgrid(tcltk::ttklabel(frame$param, text = "Parameter:", anchor = "e", width = 18), row = 0, column = 0)
      tcltk::tkgrid(wid$param, row = 0, column = 1, sticky = "we")
      tcltk::tkpack(frame$param, anchor = "e", expand = TRUE, fill = "x")
      tcltk::tkgrid.columnconfigure(frame$param, 0, weight = 0)
      tcltk::tkgrid.columnconfigure(frame$param, 1, weight = 1)
  
    tcltk::tkgrid(parFrame , row = 1, column = 0, columnspan = 2, sticky = "we", padx = c(0, 5))
  
    # Bind Parameter Frame Callbacks
    tcltk::tkbind(wid$pc, "<<ComboboxSelected>>", paramUpdate)

  geoFrame  <- tcltk::ttklabelframe(frame, text = "Geography", padding = 5)
  
    geonotebook <- tcltk::ttknotebook(geoFrame)
    
      frame$scs <- tcltk::ttkframe(geonotebook, padding = 10)
      
        frame$state <- tcltk::ttkframe(frame$scs)
          wid$state <- tcltk::ttkcombobox(frame$state, textvariable = values$state, values = comboValues(aqdm[["state"]]))
          tcltk::tkgrid(wid$state, row = 0, column = 1, sticky = "we")
          tcltk::tkgrid(tcltk::ttklabel(frame$state, text = "State:", anchor = "e", width = 15), row = 0, column = 0)
          tcltk::tkpack(frame$state, anchor = "e", expand = TRUE, fill = "x")
          tcltk::tkgrid.columnconfigure(frame$state, 0, weight = 0)
          tcltk::tkgrid.columnconfigure(frame$state, 1, weight = 1)
        
        frame$county <- tcltk::ttkframe(frame$scs)
          wid$county <- tcltk::ttkcombobox(frame$county, textvariable = values$county, values = comboValues(aqdm[["county"]]))
          tcltk::tkgrid(wid$county, row = 0, column = 1, sticky = "we")
          tcltk::tkgrid(tcltk::ttklabel(frame$county, text = "County:", anchor = "e", width = 15), row = 0, column = 0)
          tcltk::tkpack(frame$county, anchor = "e", expand = TRUE, fill = "x")
          tcltk::tkgrid.columnconfigure(frame$county, 0, weight = 0)
          tcltk::tkgrid.columnconfigure(frame$county, 1, weight = 1)
        
        frame$site <- tcltk::ttkframe(frame$scs)
          wid$site <- tcltk::ttkcombobox(frame$site, textvariable = values$site, values = comboValues(aqdm[["site"]]))
          tcltk::tkgrid(wid$site, row = 0, column = 1, sticky = "we")
          tcltk::tkgrid(tcltk::ttklabel(frame$site, text = "Site ID:", anchor = "e", width = 15), row = 0, column = 0)
          tcltk::tkpack(frame$site, anchor = "e", expand = TRUE, fill = "x")
          tcltk::tkgrid.columnconfigure(frame$site, 0, weight = 0)
          tcltk::tkgrid.columnconfigure(frame$site, 1, weight = 1)

          # Bind scs Frame Callbacks
          tcltk::tkbind(wid$state, "<<ComboboxSelected>>", countyUpdate)
          tcltk::tkbind(wid$county, "<<ComboboxSelected>>", siteUpdate)

        tcltk::tkadd(geonotebook, frame$scs, text = "State/County/Site")
      
      frame$latlon <- tcltk::ttkframe(geonotebook, padding = 10)
      
        frame$maxlat <- tcltk::ttkframe(frame$latlon)
          wid$maxlat <- tcltk::ttkentry(frame$maxlat, width = 10, textvariable = values$maxlat)
          tcltk::tkgrid(tcltk::ttklabel(frame$maxlat, text = "Max Latitude:", anchor = "e", width = 13), wid$maxlat)
        
        frame$minlat <- tcltk::ttkframe(frame$latlon)
          wid$minlat <- tcltk::ttkentry(frame$minlat, width = 10, textvariable = values$minlat)
          tcltk::tkgrid(tcltk::ttklabel(frame$minlat, text = "Min Latitude:", anchor = "e", width = 13), wid$minlat)
  
        frame$maxlon <- tcltk::ttkframe(frame$latlon)
          wid$maxlon <- tcltk::ttkentry(frame$maxlon, width = 10, textvariable = values$maxlon)
          tcltk::tkgrid(tcltk::ttklabel(frame$maxlon, text = "Max Longitude:", anchor = "e", width = 13), wid$maxlon)
        
        frame$minlon <- tcltk::ttkframe(frame$latlon)
          wid$minlon <- tcltk::ttkentry(frame$minlon, width = 10, textvariable = values$minlon)
          tcltk::tkgrid(tcltk::ttklabel(frame$minlon, text = "Min Longitude:", anchor = "e", width = 13), wid$minlon)
    
        tcltk::tkgrid(frame$maxlat, row = 0, column = 1)
        tcltk::tkgrid(frame$maxlon, row = 1, column = 0)
        tcltk::tkgrid(frame$minlon, row = 1, column = 2)
        tcltk::tkgrid(frame$minlat, row = 2, column = 1)
  
        tcltk::tkadd(geonotebook, frame$latlon, text = "Latitude/Longitude")
  
      frame$geoother <- tcltk::ttkframe(geonotebook, padding = 10)
  
        frame$cbsa <- tcltk::ttkframe(frame$geoother)
          wid$cbsa <- tcltk::ttkcombobox(frame$cbsa, textvariable = values$cbsa, values = comboValues(aqdm[["cbsa"]]))
          tcltk::tkgrid(wid$cbsa, row = 0, column = 1, sticky = "we")
          tcltk::tkgrid(tcltk::ttklabel(frame$cbsa, text = "CBSA:", anchor = "e", width = 15), row = 0, column = 0)
          tcltk::tkpack(frame$cbsa, anchor = "e", expand = TRUE, fill = "x")
          tcltk::tkgrid.columnconfigure(frame$cbsa, 0, weight = 0)
          tcltk::tkgrid.columnconfigure(frame$cbsa, 1, weight = 1)
        
        frame$csa <- tcltk::ttkframe(frame$geoother)
          wid$csa <- tcltk::ttkcombobox(frame$csa, textvariable = values$csa, values = comboValues(aqdm[["csa"]]))
          tcltk::tkgrid(wid$csa, row = 0, column = 1, sticky = "we")
          tcltk::tkgrid(tcltk::ttklabel(frame$csa, text = "CSA:", anchor = "e", width = 15), row = 0, column = 0)
          tcltk::tkpack(frame$csa, anchor = "e", expand = TRUE, fill = "x")
          tcltk::tkgrid.columnconfigure(frame$csa, 0, weight = 0)
          tcltk::tkgrid.columnconfigure(frame$csa, 1, weight = 1)
  
        tcltk::tkadd(geonotebook, frame$geoother, text = "Other Geography")
  
      tcltk::tkpack(geonotebook, expand = TRUE, fill = "both")
  
    tcltk::tkgrid(geoFrame , row = 2, column = 0, columnspan = 2, sticky = "we", padx = c(0, 5))
  
  
  buttFrame <- tcltk::ttkframe(frame, padding = 5)
  
    frame$butt <- tcltk::ttkframe(buttFrame)
      wid$cancel <- tcltk::ttkbutton(frame$butt, text = "Cancel", width = 15, command = function() tcltk::tkdestroy(window))
      wid$copy <- tcltk::ttkbutton(frame$butt, text = "Create Function", width = 15, command = function() tcltk::tclvalue(done) <- 2)
      wid$defaults <- tcltk::ttkbutton(frame$butt, text = "Set Defaults", width = 15, command = function() tcltk::tclvalue(done) <- 3)
      wid$request <- tcltk::ttkbutton(frame$butt, text = "Request Data", width = 15, command = function() tcltk::tclvalue(done) <- 4)
    
      tcltk::tkgrid(wid$cancel, wid$copy, wid$defaults, wid$request, sticky = "e")
  
    tcltk::tkpack(frame$butt, anchor = "e")
  
  
  tcltk::tkgrid(buttFrame, row = 3, column = 0, columnspan = 3, sticky = "we")

##### Control Output from the GUI #####
  
  # Variable to store gui state
  done <- tcltk::tclVar(0);
  tcltk::tkbind(window, "<Destroy>", function() tcltk::tclvalue(done) <- 1)
  tcltk::tkraise(window)
  tcltk::tkfocus(window)
  tcltk::tcl("wm", "attributes", window, topmost=TRUE)
  tcltk::tcl("wm", "attributes", window, topmost=FALSE)
  tcltk::tkwait.variable(done)

  done <- tcltk::tclvalue(done)

  if(done == 1) {
    op <- NULL
  } else {
        
    params <- lapply(validNames, function(n) {
      x <- tcltk::tclvalue(values[[n]])
      x <- strsplit(x, " - ")[[1]][1]
      return(x)
    })
    names(params) <- validNames
    params <- params[!is.na(params)]
    params <- verifyVariables(params)
    
    func <- paste0("getAQDMdata(", paste(names(params), params, sep = " = \"", collapse = "\", "), "\"")
    if(tcltk::tclvalue(values$requestType) == "rawData") {
      func <- paste0(func, ", synchronous = TRUE)")
    } else {
      func <- paste0(func, ", synchronous = FALSE)")
    }
        
    if(done == 2) {
      message("Function text returned")
      op <- func
      message(op)
      
    } else if(done == 3) {
      message("Defaults Set")
      setAQDMdefaults(params)
      op <- NULL
      
    } else if(done == 4) {
      message("Data Requested")
      op <- eval(parse(text = func))
      
    } else {
      op <- NULL
      
    }
    
    tcltk::tkdestroy(window)
    
  }

  return(invisible(op))
  

##### END openAQDMgui #####
}
