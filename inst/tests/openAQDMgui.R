library(tcltk)
library(raqdm)


openAQDMgui <- function() {

  # List of variables in the AQDM environment
  env.variables <- ls(aqdm)
  # Any default variables that have been set
  def.variables <- getOption("raqdmOptions")
  
  # List of all the tcl variables that are linked to widgets.
  tcl.variables <- list()
  
  tk.widgets <- list()

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
        type <- "combo"
      }
    } else {
      d = ""
    }
    
    tcl.variables[[n]] <- try(tclVar(d), silent = TRUE)
    if(class(tcl.variables[[n]]) == "try-error") tcl.variables[[n]] <- tclVar("")
    
  }



  # Function to subset parameter combobox by pc combobox selection
  paramUpdate <- function() {
    
    x <- tclvalue(pc)
    p <- tclvalue(param)
    
    if(x != "") {
      x <- strsplit(x, " - ")[[1]][1]
      params <- getAQDMlist("param", pc = x)
    }
    
    newParams <- paste(params[, 1], params[,2], sep = " - ")
    tkconfigure(widgets$param, values = newParams)
    
    if(!(p %in% newParams)) {
      tclvalue(param) <- ""
    }
    
  }
  
  window <- tktoplevel()
  tkwm.title(window, "AQDM Data Request")
  frame <- ttkframe(window, padding = 10)
  tkpack(frame, expand = TRUE, fill = "both")

  # Code for creating the Authentication Inputs
  authframe <- ttklabelframe(frame, padding = 5, relief = "flat", text = "Authentication")
  tkpack(authframe, expand = TRUE, fill = "x")

  tk.widgets$user <- createWidget(authframe, "entry", "Username:", tcl.variables$user)
  tk.widgets$pw <- createWidget(authframe, "entry", "Password:", tcl.variables$pw)

  parframe <- ttklabelframe(frame, padding = 5, relief = "flat", text = "Parameters")
  tkpack(parframe, expand = TRUE, fill = "x")

  tk.widgets$pc <- createWidget(parframe, "combo", "Parameter Class:", tcl.variables$pc, paste(aqdm$pc[, 1], "-", aqdm$pc[, 2]))
  tk.widgets$param <- createWidget(parframe, "combo", "Parameter:", tcl.variables$param, paste(aqdm$param[, 1], "-", aqdm$param[, 2]))
  tkbind(tk.widgets$pc, "<<ComboboxSelected>>", paramUpdate)

  timeframe <- ttklabelframe(frame, padding = 5, relief = "flat", text = "Time")
  tkpack(timeframe, expand = TRUE, fill = "x")

  sdate <- ttkentry(timeframe, width = 12)
  edate <- ttkentry(timeframe, width = 12)
  csdate <- ttkentry(timeframe, width = 12)
  cedate <- ttkentry(timeframe, width = 12)
  tkgrid(tklabel(timeframe, text = "Sampling Dates: "), sdate, edate)
  tkgrid(tklabel(timeframe, text = "Change Dates: "), csdate, cedate)
              

}