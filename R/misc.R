validNames <- c("user", "pw", "state", "county", "site", "pc", "param", 
                "format", "cbsa", "csa", "duration", "bdate", "edate", "cbdate",
                "cedate", "minlat", "maxlat", "minlon", "maxlon", "frmonly")
defaultsPath <- normalizePath(paste0(system.file(package="raqdm"), "/defaults.rda"))
baseURL <- "https://ofmext.epa.gov/AQDMRS/ws/"

.onAttach <- function(...) {

  load(defaultsPath)
  options("raqdmOptions" = aqdmDefaults)
  
  if(is.null(aqdmDefaults$user)) {
    packageStartupMessage("\nU.S. EPA's Air Quaility Data Mart requires free registration. Please visit\n\n",
                            "   http://www.epa.gov/airdata/tas_Data_Mart_Registration.html\n\n",
                            "for information on registering. Once registered use\n\n",
                            "   setAQDMUser()\n\n",
                            "to set your username and password for use with raqdm.")
  }
  
}

