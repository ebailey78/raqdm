library(tcltk)
library(raqdm)
setAQDMdefaults(user = "ebailey@idem.in.gov", pw = "bolefox64", param = 44201, state = "18", county = "89", bdate=as.Date('2013-01-01'), edate='20131231')

aqdm <- new.env()

validLists <- c("state", "pc", "param", "format", "cbsa", "csa", "dur")
env.var <- ls(aqdm)
def.var <- getOption("raqdmOptions")

for(l in validLists) {
   
  if(!(l %in% env.var)) {
    aqdm[[l]] <- getAQDMlist(l)
  }
  
}

user <- try(tclVar(def.var$user), silent = TRUE)
if(class(user) == "try-error") user <- tclVar("")
pw <- try(tclVar(def.var$pw), silent = TRUE)
if(class(pw) == "try-error") pw <- tclVar("")
pc <- def.var$pc
if(!is.null(pc)) {
  r <- aqdm$pc[aqdm$pc[,1] == pc, ]
  pc <- tclVar(paste(r[, 1], "-", r[,2]))
} else {
  pc <- tclVar("")
}
if(class(pc) == "try-error") pc <- tclVar("")
param <- def.var$param
if(!is.null(param)) {
  r <- aqdm$param[aqdm$param[,1] == param, ]
  param <- tclVar(paste(r[,1], "-", r[,2]))
} else {
  param <- tclVar("")
}




createWidget <- function(parent, type = "entry", label, value, options, width = "40") {
  
  frm <- ttkframe(parent)
  lbl <- tklabel(frm, text = label, anchor = "e", width = nchar(label) + 1)
  if(type == "entry") {
    wgt <- ttkentry(frm, textvariable = value, width = width)
  } else if(type == "combo") {
    wgt <- ttkcombobox(frm, textvariable = value, values = options, width = width)
  }
  tkgrid(lbl, wgt)
  tkpack(frm, anchor = "e")
  return(wgt)

}

paramUpdate <- function() {
  
  x <- tclvalue(pc)
  param <- tclvalue(param)
  
  if(x != "") {
    x <- strsplit(x, " - ")[[1]][1]
    params <- getAQDMlist("param", pc = x)
  }
  
  newParams <- paste(params[, 1], params[,2], sep = " - ")
  tkconfigure(widgets$param, values = newParams)
  
  if(!(param %in% newParams)) {
    tclvalue(param) <- ""
  }
     
}

widgets <- list()

window <- tktoplevel()
tkwm.title(window, "AQDM Data Request")
frame <- ttkframe(window, padding = 10)
tkpack(frame, expand = TRUE, fill = "both")

authframe <- ttklabelframe(frame, padding = 5, relief = "flat", text = "Authentication")
tkpack(authframe, expand = TRUE, fill = "x")

widgets$user <- createWidget(authframe, "entry", "Username:", user)
widgets$pw <- createWidget(authframe, "entry", "Password:", pw)

parframe <- ttklabelframe(frame, padding = 5, relief = "flat", text = "Parameters")
tkpack(parframe, expand = TRUE, fill = "x")

widgets$pc <- createWidget(parframe, "combo", "Parameter Class:", pc, paste(aqdm$pc[, 1], "-", aqdm$pc[, 2]))
widgets$param <- createWidget(parframe, "combo", "Parameter:", param, paste(aqdm$param[, 1], "-", aqdm$param[, 2]))
tkbind(widgets$pc, "<<ComboboxSelected>>", paramUpdate)








timeframe <- ttklabelframe(frame, padding = 5, relief = "flat", text = "Time")
tkpack(timeframe, expand = TRUE, fill = "x")

sdate <- ttkentry(timeframe, width = 12)
edate <- ttkentry(timeframe, width = 12)
csdate <- ttkentry(timeframe, width = 12)
cedate <- ttkentry(timeframe, width = 12)
tkgrid(tklabel(timeframe, text = "Sampling Dates: "), sdate, edate)
tkgrid(tklabel(timeframe, text = "Change Dates: "), csdate, cedate)
              


tcl(button, "state", "disabled")
tcl(button, "state", "!disabled")
