#'@export
setAQDMuser <- function(username, password, save = FALSE) {
 
  aqdmDefaults <- getOption(raqdmOptions)
  
  aqdmDefaults$user <- username
  aqdmDefaults$pw <- password
  
  if(save) {
    save(aqdmDefaults, file = defaultsPath)
  }
  
  options(raqdmOptions = aqdmDefaults)
  
  message("\nAQDM user information set for ", username, ".")
  
  return(invisible(TRUE))
  
}

