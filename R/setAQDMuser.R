#'setAQDMuser
#'
#'Convienince function for setting your username and password for accessing the
#'Air Quality Data Mart.
#'
#'@param username Your AQDM username (your email address)
#'@param password Your AQDM password
#'@param save \code{logical} Should the username/password be saved for future sessions?
#'
#'@details
#'You can set user and pw from the \code{\link{setAQDMDefaults}} function also.
#'
#'@return
#'This function returns \code{TRUE} invisibly if there are no problems.
#'
#'@examples
#'  setAQDMuser("fake@@email.com", "pass123") 
#'  
#'@seealso
#'  \href{http://www.epa.gov/airdata/toc.html}{Query Air Data (QAD) User's Guide}
#'
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

