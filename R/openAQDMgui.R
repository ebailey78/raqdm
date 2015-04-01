library(tcltk)
library(raqdm)

# # Parameters set for testing, DELETE BEFORE PRODUCTION
# setAQDMdefaults(user = "ebailey@idem.in.gov", pw = "bolefox64", param = 44201, state = "18", county = "89", bdate=as.Date('2013-01-01'), edate='20131231')
# aqdm <- new.env()
# validLists <- c("state", "county", "site", "pc", "param", "format", "cbsa", "csa", "dur")
# validNames <- c("user", "pw", "state", "county", "site", "pc", "param", 
#                 "format", "cbsa", "csa", "dur", "bdate", "edate", "cbdate",
#                 "cedate", "minlat", "maxlat", "minlon", "maxlon", "frmonly")

#'@export
openAQDMgui <- function() {

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
   
    return(paste(df[,1], df[,2], sep = " - "))
    
  }
  
  # Loop through lists and if they haven't already been requested this session
  # request them and add them to the aqdm environment.
  for(l in validLists) {
    if(!(l %in% env.variables) & !(l %in% c("site", "county"))) {
      aqdm[[l]] <- getAQDMlist(l)
    } else if(l == "county" & !is.null(def.variables$state)) {
      aqdm[[l]] <- getAQDMlist(l, state = def.variables$state)
    } else if(l == "site" & !is.null(def.variables$state) & !is.null(def.variables$county)) {
      aqdm[[l]] <- getAQDMlist(l, state = def.variables$state, county = def.variables$county)
    } else {
      aqdm[[l]] <- NULL
    }
  }
  
  # Loop through all variables making tcl variables and asign their default value,
  # if present.
  for(i in seq_along(validNames)) {
    
    n <- validNames[i]
    d <- def.variables[[n]]
    type <- "entry"
    
    if(!is.null(d)) {
      if(n %in% validLists) {
        r <- aqdm[[n]][aqdm[[n]][, 1] == d, ]
        d <- paste(r[, 1], r[, 2], sep = " - ")
      }
    } else {
      d = ""
    }
    
    if("Date" %in% class(d) | "POSIXt" %in% class(d)) {
      d <- format(d, "%Y%m%d")
    }      
      
    values[[n]] <- try(tclVar(d), silent = TRUE)
    if(class(values[[n]]) == "try-error") values[[n]] <- tclVar("")
    
  }
  
  window <- tktoplevel()
  tkwm.title(window, "AQDM Data Request")
  frame <- ttkframe(window, padding = 10)
  tkpack(frame, expand = TRUE, fill = "both")
  
  authFrame <- ttklabelframe(frame, text = "Authentication", padding = 5)
  
    frame$user <- ttkframe(authFrame)
    wid$user <- ttkentry(frame$user, width = 30, textvariable = values$user)
    tkgrid(ttklabel(frame$user, text = "Username:", anchor = "e"), wid$user)
    tkpack(frame$user, anchor = "e")
    
    frame$pw <- ttkframe(authFrame)
    wid$pw <- ttkentry(frame$pw, width = 30, textvariable = values$pw)
    tkgrid(ttklabel(frame$pw, text = "Password:", anchor = "e"), wid$pw)
    tkpack(frame$pw, anchor = "e")
  
    tkgrid(authFrame, row = 0, column = 0, sticky = "we")
  
  
  dateFrame <- ttklabelframe(frame, text = "Date Ranges", padding = 5)
  
    frame$sampDate <- ttkframe(dateFrame)
    wid$bdate <- ttkentry(frame$sampDate, width = 10, textvariable = values$bdate)
    wid$edate <- ttkentry(frame$sampDate, width = 10, textvariable = values$edate)
    tkgrid(ttklabel(frame$sampDate, text = "Sampling Dates:", anchor = "e"), wid$bdate, wid$edate)
    tkpack(frame$sampDate, anchor = "e")
    
    frame$changeDate <- ttkframe(dateFrame)
    wid$cbdate <- ttkentry(frame$changeDate, width = 10, textvariable = values$cbdate)
    wid$cedate <- ttkentry(frame$changeDate, width = 10, textvariable = values$cedate)
    tkgrid(ttklabel(frame$changeDate, text = "Change Dates:", anchor = "e"), wid$cbdate, wid$cedate)
    tkpack(frame$changeDate, anchor = "e")
  
    tkgrid(dateFrame, row = 0, column = 1, sticky = "we")
  
  parFrame  <- ttklabelframe(frame, text = "Parameters", padding = 5)
  
    frame$pc <- ttkframe(parFrame)
      wid$pc <- ttkcombobox(frame$pc, width = 40, textvariable = values$pc, values = comboValues(aqdm[["pc"]]))
      tkgrid(wid$pc, row = 0, column = 1, sticky = "we")
      tkgrid(ttklabel(frame$pc, text = "Parameter Class:", anchor = "e", width = 18), row = 0, column = 0)
      tkpack(frame$pc, anchor = "e", expand = TRUE, fill = "x")
      tkgrid.columnconfigure(frame$pc, 0, weight = 0)
      tkgrid.columnconfigure(frame$pc, 1, weight = 1)
    
    frame$param <- ttkframe(parFrame)
      wid$param <- ttkcombobox(frame$param, width = 40, textvariable = values$param, values = comboValues(aqdm[["param"]]))
      tkgrid(ttklabel(frame$param, text = "Parameter:", anchor = "e", width = 18), row = 0, column = 0)
      tkgrid(wid$param, row = 0, column = 1, sticky = "we")
      tkpack(frame$param, anchor = "e", expand = TRUE, fill = "x")
      tkgrid.columnconfigure(frame$param, 0, weight = 0)
      tkgrid.columnconfigure(frame$param, 1, weight = 1)
  
    tkgrid(parFrame , row = 1, column = 0, columnspan = 2, sticky = "we")
  
  geoFrame  <- ttklabelframe(frame, text = "Geography", padding = 5)
  
    geonotebook <- ttknotebook(geoFrame)
    
      frame$scs <- ttkframe(geonotebook, padding = 10)
      
        frame$state <- ttkframe(frame$scs)
          wid$state <- ttkcombobox(frame$state, textvariable = values$state, values = comboValues(aqdm[["state"]]))
          tkgrid(wid$state, row = 0, column = 1, sticky = "we")
          tkgrid(ttklabel(frame$state, text = "State:", anchor = "e", width = 15), row = 0, column = 0)
          tkpack(frame$state, anchor = "e", expand = TRUE, fill = "x")
          tkgrid.columnconfigure(frame$state, 0, weight = 0)
          tkgrid.columnconfigure(frame$state, 1, weight = 1)
        
        frame$county <- ttkframe(frame$scs)
          wid$county <- ttkcombobox(frame$county, textvariable = values$county, values = comboValues(aqdm[["county"]]))
          tkgrid(wid$county, row = 0, column = 1, sticky = "we")
          tkgrid(ttklabel(frame$county, text = "County:", anchor = "e", width = 15), row = 0, column = 0)
          tkpack(frame$county, anchor = "e", expand = TRUE, fill = "x")
          tkgrid.columnconfigure(frame$county, 0, weight = 0)
          tkgrid.columnconfigure(frame$county, 1, weight = 1)
        
        frame$site <- ttkframe(frame$scs)
          wid$site <- ttkcombobox(frame$site, textvariable = values$site, values = comboValues(aqdm[["site"]]))
          tkgrid(wid$site, row = 0, column = 1, sticky = "we")
          tkgrid(ttklabel(frame$site, text = "Site ID:", anchor = "e", width = 15), row = 0, column = 0)
          tkpack(frame$site, anchor = "e", expand = TRUE, fill = "x")
          tkgrid.columnconfigure(frame$site, 0, weight = 0)
          tkgrid.columnconfigure(frame$site, 1, weight = 1)
      
        tkadd(geonotebook, frame$scs, text = "State/County/Site")
      
      frame$latlon <- ttkframe(geonotebook, padding = 10)
      
        frame$maxlat <- ttkframe(frame$latlon)
          wid$maxlat <- ttkentry(frame$maxlat, width = 10, textvariable = values$maxlat)
          tkgrid(ttklabel(frame$maxlat, text = "Max Latitude:", anchor = "e", width = 13), wid$maxlat)
        
        frame$minlat <- ttkframe(frame$latlon)
          wid$minlat <- ttkentry(frame$minlat, width = 10, textvariable = values$minlat)
          tkgrid(ttklabel(frame$minlat, text = "Min Latitude:", anchor = "e", width = 13), wid$minlat)
  
        frame$maxlon <- ttkframe(frame$latlon)
          wid$maxlon <- ttkentry(frame$maxlon, width = 10, textvariable = values$maxlon)
          tkgrid(ttklabel(frame$maxlon, text = "Max Longitude:", anchor = "e", width = 13), wid$maxlon)
        
        frame$minlon <- ttkframe(frame$latlon)
          wid$minlon <- ttkentry(frame$minlon, width = 10, textvariable = values$minlon)
          tkgrid(ttklabel(frame$minlon, text = "Min Longitude:", anchor = "e", width = 13), wid$minlon)
    
        tkgrid(frame$maxlat, row = 0, column = 1)
        tkgrid(frame$maxlon, row = 1, column = 0)
        tkgrid(frame$minlon, row = 1, column = 2)
        tkgrid(frame$minlat, row = 2, column = 1)
  
        tkadd(geonotebook, frame$latlon, text = "Latitude/Longitude")
  
      frame$geoother <- ttkframe(geonotebook, padding = 10)
  
        frame$cbsa <- ttkframe(frame$geoother)
          wid$cbsa <- ttkcombobox(frame$cbsa, textvariable = values$cbsa, values = comboValues(aqdm[["cbsa"]]))
          tkgrid(wid$cbsa, row = 0, column = 1, sticky = "we")
          tkgrid(ttklabel(frame$cbsa, text = "CBSA:", anchor = "e", width = 15), row = 0, column = 0)
          tkpack(frame$cbsa, anchor = "e", expand = TRUE, fill = "x")
          tkgrid.columnconfigure(frame$cbsa, 0, weight = 0)
          tkgrid.columnconfigure(frame$cbsa, 1, weight = 1)
        
        frame$csa <- ttkframe(frame$geoother)
          wid$csa <- ttkcombobox(frame$csa, textvariable = values$csa, values = comboValues(aqdm[["csa"]]))
          tkgrid(wid$csa, row = 0, column = 1, sticky = "we")
          tkgrid(ttklabel(frame$csa, text = "CSA:", anchor = "e", width = 15), row = 0, column = 0)
          tkpack(frame$csa, anchor = "e", expand = TRUE, fill = "x")
          tkgrid.columnconfigure(frame$csa, 0, weight = 0)
          tkgrid.columnconfigure(frame$csa, 1, weight = 1)
  
        tkadd(geonotebook, frame$geoother, text = "Other Geography")
  
      tkpack(geonotebook, expand = TRUE, fill = "both")
  
    tkgrid(geoFrame , row = 2, column = 0, columnspan = 2, sticky = "we")
  
  
  buttFrame <- ttkframe(frame, padding = 5)
  
    frame$butt <- ttkframe(buttFrame)
      wid$cancel <- ttkbutton(frame$butt, text = "Cancel", width = 13)
      wid$copy <- ttkbutton(frame$butt, text = "Copy Function", width = 13)
      wid$defaults <- ttkbutton(frame$butt, text = "Set Defaults", width = 13)
      wid$request <- ttkbutton(frame$butt, text = "Request Data", width = 13)
    
      tkgrid(wid$cancel, wid$copy, wid$defaults, wid$request, sticky = "e")
  
    tkpack(frame$butt, anchor = "e")
  
  
  tkgrid(buttFrame, row = 3, column = 0, columnspan = 2, sticky = "we")

}
