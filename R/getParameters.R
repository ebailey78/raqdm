getParameters <- function() {
  
  params <- get("default.params", cache)
  pass <- FALSE
  
  n <- names(params)
  
  validateRequest <- function() {
    
    valid = TRUE
    
    checkll <- function() {
      
      ok = "bad"
      
      if((tclvalue(v.minlat) != "" & tclvalue(v.maxlat) != "" & tclvalue(v.minlon) != "" & tclvalue(v.maxlon) != "")) ok="good"
      if((tclvalue(v.minlat) == "" & tclvalue(v.maxlat) == "" & tclvalue(v.minlon) == "" & tclvalue(v.maxlon) == "")) ok="empty"
      
      ok
      
    }
    
    if(tclvalue(v.user) == "" | tclvalue(v.pw) == "") {
      valid = FALSE
      tkmessageBox(message = "Please provide both a user name and a password.")
    } else if(tclvalue(v.bdate) == "" | tclvalue(v.edate) == "") {
      valid = FALSE
      tkmessageBox(message = "Please provide a begin date and an end datv.")
    } else if(tclvalue(v.bdate) > tclvalue(v.edate)) {
      tkmessageBox(message = "The End Date should be after the Begin Date.")
    } else if(tclvalue(v.pc) == "" & tclvalue(v.param) == "") {
      valid = FALSE
      tkmessageBox(message = "Either a parameter class or a parameter code is required.")
    } else if(checkll() == "bad") {
      valid = FALSE
      tkmessageBox(message = "If lat/lon are provided, all four sides of bounding box must be included.")
    } else if(checkll() == "empty" & tclvalue(v.state) == "" & tclvalue(v.cbsa) == "" & tclvalue(v.csa) == "") {
      valid = FALSE
      tkmessageBox(message = "Some sort of geographic bounding must be provided.")
    }
    
    valid
    
  }
  
  getComboList <- function(name, quals) {

    if(!missing(quals)) {

      qs <- lapply(quals, function(x) {
        sel <- tclvalue(eval(parse(text = paste0("v.", x))))
        sel <- substr(sel, 1, regexpr(" - ", sel) - 1)
        paste0(x, " = \"", sel, "\"")
      })

      qs <- eval(parse(text = paste0("list(", paste(qs, collapse = ", "), ")")))

      l <- getValues(name, qs, stop.on.error = FALSE)

    } else {

      l <- getValues(name, stop.on.error=FALSE)

    }

    if(is.data.frame(l)) l <- c("", paste(l[, 1], "-", l[, 2]))

    l

  }

  updateEntry <- function(name) {
    
    sel <- tclvalue(eval(parse(text = paste0("v.", name))))
    params[name] <<- sel
    
    return(tclVar(TRUE))
    
  }
  
  cbSwitch <- function(child, parent) {

    x <- getComboList(child, parent)

    if(params[[child]] != "" & params[[parent[1]]] == tclvalue(eval(parse(text=paste0("v.", parent[1]))))) {
      y = grep(paste0("^", params[[child]]), x)
    }
    
    tkconfigure(eval(parse(text=paste0("e.", child))), state="readonly", values=x)
    if(params[[child]] %in% x) {
      tkset(eval(parse(text=paste0("e.", child))), params[[child]])
    } else {
      tkset(eval(parse(text=paste0("e.", child))), "")
      params[[child]] <<- ""
    }
    
    updateEntry(parent[1])
    
    TRUE
    
  }
  
  #Labels
  ln <- c("User Name", "Password", "Output Format", "Parameter Class", 
          "Parameter Code", "Begin Date", "End Date", "Min. Latitude",
          "Max. Latitude", "Min. Longitude", "Max. Longitude", "State", 
          "County", "Site", "CBSA", "CSA", "Duration", "FRM Only")
  
  ht <- c("(Required) The username assigned to you by EPA AQDM. See the help file for the rAQDM package for help getting a username and password.",
          "(Required) The password assigned to you by EPA AQDM. See the help file for the rAQDM package for help getting a username and password.",
          "(Optional) The output format for the data request. It is recommended that you leave this blank unless you specifically need a special format.",
          "(Optional) Selecting a parameter class will request all parameters that fall within that class. It will also subset the parameter code combobox. You must select either a parameter class or a parameter code.",
          "(Optional) Selecting a parameter code will request only that specific parameter. You must select either a parameter class or a parameter code.",
          "(Required) The earliest date for the data request. Should be in 'yyyymmdd' format without dashes or backslashes.",
          "(Required) The latest date for the data request. Should be in 'yyyymmdd' format without dashes or backslashes.",
          "(Optional) The southern border of a bounding box. All four borders must be specified, if any are. The total degrees should not exceed 50.",
          "(Optional) The northern border of a bounding box. All four borders must be specified, if any are. The total degrees should not exceed 50.",
          "(Optional) The eastern border of a bounding box. All four borders must be specified, if any are. The total degrees should not exceed 50.",
          "(Optional) The western border of a bounding box. All four borders must be specified, if any are. The total degrees should not exceed 50.",
          "(Optional) The state from which to request data. If you select a state, you may then pick a county from within that state.",
          "(Optional) The county from which to request data. You must select a state before you may select a county. If you select a county, you may then select a specific site within that county.",
          "(Optional) The specific site whose data is requested. You must select a county before this option is available.",
          "(Optional) Core Based Statistical Area as defined by the U.S. Office of Management and Budget. This is an optional way off selecting a geographic region for you data request.",
          "(Optional) Combined Statistical Area as defined by the U.S. Census Bureau. This is an optional way to define a geographic region for your data request.",
          "(Optional) The sampling duration for which you are interested.",
          "(Optional) Should only data collected with Federal Reference or Equivalent Methods be included?")
  
  #Input Types
  ty <- c("text", "text", "combo", "combo", "combo", "text", "text", "text",
          "text", "text", "text", "combo", "combo", "combo", "combo", "combo",
          "combo", "check")
  
  #Create Tcl/Tk gui for parameter selection
  tt <- tktoplevel(padx=8, pady=5)
  tkwm.deiconify(tt)
  tkgrab.set(tt)
  tkfocus(tt)
  tkwm.title(tt, "Build Parameters List")
  
  for(i in seq_along(n)) {
    
    # Variables for storing contents of input widgets
    eval(parse(text = paste0("v.", n[i], " <- tclVar(\"", params[i], "\")")))
    # Varaibles for storing state of "set default" checkbuttons
    eval(parse(text = paste0("kv.", n[i], " <- tclVar(", !(params[[i]] == ""), ")")))
    
    # Create labels
    eval(parse(text = paste0("l.", n[i], " <- tklabel(tt, text = \"", 
                             ln[i], "\", pady = 3)")))
    # Create input widgets
    if(ty[i] == "text") {
      eval(parse(text = paste0("e.", n[i], " <- tkentry(tt, width = 85, 
                             textvariable = v.", n[i], ", validate=\"focusout\", validatecommand = function() updateEntry(\"", n[i], "\"))")))
    } else if(ty[i] == "combo") {
      eval(parse(text = paste0("e.", n[i], " <- ttkcombobox(tt, width = 82, 
                             values = getComboList(n[i]),
                             state = \"readonly\", textvariable = v.", n[i], ")")))
    } else if(ty[i] == "check") {
      eval(parse(text = paste0("e.", n[i], " <- tkcheckbutton(tt, onvalue = 'y', 
                             offvalue = '', command = function() updateEntry(\"", n[i], "\"), 
                             variable = v.", n[i], ")")))
    }
    
    # Add help buttons
    eval(parse(text = paste0("h.", n[i], " <- tkbutton(tt, text = ' ? ', padx=3, command=function() {tkmessageBox(message = \"", ht[i],"\")})")))
    
    # Create "set default" checkbuttons
    eval(parse(text = paste0("k.", n[i], " <- tkcheckbutton(tt, onvalue = TRUE, 
                           offvalue = FALSE, variable = kv.", n[i], ")")))
    
    # Add widgets to gui
    eval(parse(text = paste0("tkgrid(l.", n[i], ", e.", n[i], ", k.", n[i], ", h.", n[i], ")")))
    # Align widgets
    tkgrid.configure(eval(parse(text = paste0("l.", n[i]))), sticky="e")
    tkgrid.configure(eval(parse(text = paste0("e.", n[i]))), sticky="w")
    
  }
  
  cbSwitch("county", "state")
  cbSwitch("site", c("state", "county"))
  cbSwitch("param", "pc")
  
  go <- function() {
    if(validateRequest()) {
      pass <<- TRUE
      p2 <- params
      for(i in seq_along(p2)) {
        if(tclvalue(eval(parse(text = paste0("kv.", n[i])))) == "0") p2[i] = ""
      }
      assign("default.params", p2, cache)      
      tkgrab.release(tt)
      tkdestroy(tt)
    }
  }
  
  e.go <- tkbutton(tt, text="Okay", width = 10, command = go)
  tkgrid(e.go, pady = 10, columnspan = 4)
  tkgrid.configure(e.go, sticky="e")
  
  # Bind actions to keep params list up-to-date
  tkbind(e.state, "<<ComboboxSelected>>", function() cbSwitch("county", "state"))
  tkbind(e.county, "<<ComboboxSelected>>", function() cbSwitch("site", c("county", "state")))
  tkbind(e.pc, "<<ComboboxSelected>>", function() cbSwitch("param", "pc"))
  tkbind(e.format, "<<ComboboxSelected>>", function() updateEntry("format"))
  tkbind(e.param, "<<ComboboxSelected>>", function() updateEntry("param"))
  tkbind(e.site, "<<ComboboxSelected>>", function() updateEntry("site"))
  tkbind(e.cbsa, "<<ComboboxSelected>>", function() updateEntry("cbsa"))
  tkbind(e.csa, "<<ComboboxSelected>>", function() updateEntry("csa"))
  tkbind(e.dur, "<<ComboboxSelected>>", function() updateEntry("dur"))
  
  tkwait.window(tt)

  l <- list()
  class(l) <- "aqdmparams"
  
  for(i in seq_along(n)) {
    if(params[i] != "") {
      if(ty[i] == "combo") params[i] = substr(params[i], 1, regexpr(" - ", params[i]) - 1)
      l[n[i]] <- params[[i]]
    }
  }

  if(pass) {
    return(l)
  } else {
    stop("Parameter selection cancelled")
  }
  
}