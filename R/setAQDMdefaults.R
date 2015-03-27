#'@export
setAQDMdefaults <- function(..., save = TRUE) {
  
  x <- list(...)
  
  if(length(x) == 1 & length(x[[1]]) > 1 & is.list(x[[1]])) {
    x <- x[[1]]
  }
  
  aqdmDefaults <- getOption("raqdmOptions")
  
  for(i in seq(length(x))) {
    if(names(x)[i] %in% validNames) {
      aqdmDefaults[[names(x)[i]]] <- x[[i]]
    } else {
      warning(names(x)[i], " not a valid AQDM parameter. Skipping...")
    }
  }
  
  options("raqdmOptions" = aqdmDefaults)
  
  if(save) {
    save(aqdmDefaults, file = defaultsPath)
  }
  
  return(invisible(TRUE))
  
}
