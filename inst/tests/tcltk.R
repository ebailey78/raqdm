library(tcltk)

aqdm <- new.env()

validLists <- c("state", "pc", "param", "format", "cbsa", "csa", "dur")
env.var <- ls(aqdm)
def.var <- getOption("raqdmOptions")

ui <- list(user = list(tkentry, "Username:"),
           pw   = list(tkentry, "Password:"),
           format = list(ttkcombobox, "Output Format:", aqdm$format),
           
)

for(l in validLists) {
   
  if(!(l %in% env.var)) {
    aqdm[[l]] <- getAQDMlist(l)
  }
  
}

window <- tktoplevel()
tkwm.title(window, "AQDM Data Request")
frame <- ttkframe(window, padding = 10)
tkpack(frame, expand = TRUE, fill = "both")

authframe <- ttklabelframe(frame, padding = 5, relief = "flat", text = "Authentication")
tkpack(authframe, expand = TRUE, fill = "x")

user <- ttkentry(authframe)
tkgrid(tklabel(authframe, text = "Username: "), user)
pw <- ttkentry(authframe)
tkgrid(tklabel(authframe, text = "Password: "), pw)

parframe <- ttklabelframe(frame, padding = 5, relief = "flat", text = "Parameters")
tkpack(parframe, expand = TRUE, fill = "x")

pc <- ttkcombobox(parframe, values = aqdm$pc[,2], width = 50)
tkgrid(tklabel(parframe, text = "Parameter Class: "), pc)

param <- ttkcombobox(parframe, values = aqdm$param[,2], width = 50)
tkgrid(tklabel(parframe, text = "Parameter: "), param)

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
