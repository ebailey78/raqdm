\name{defaults}
\alias{setDefaults}
\alias{getDefaults}
\alias{clearDefaults}
\title{
Functions to interface with default parameters
}
\description{
Functions for setting, getting, and clearing the default query parameters for an
AQDM data pull
}
\usage{
getDefaults()
setDefaults(x)
clearDefaults(x)
}

\arguments{
  \item{x}{
  either a vector or a named list of parameters (See details)
}
}
\details{

  For \code{setDefaults}, \code{x} needs to be a named list of parameters that you wish 
  to set. \cr \cr
  For \code{clearDefaults}, \code{x} can be a list or vector of parameter names to you 
  wish to reset. If \code{x} is missing, all default parameters will be reset.

}
\value{
  \code{getDefaults} will return a list of default parameters for AQDM data 
  queries. \cr \cr
  The other functions are used for their side effects of changing the default 
  query parameters for the package and return no meaningful value.
}
\references{
  \href{http://www.epa.gov/airdata/toc.html}{Query Air Data (QAD) User's Guide}
}
\author{
Eric Bailey
}
\note{

  Accessing data from EPA's AQDM requires a username and password. Anyone can 
  get a username and password by emailing aqsdatamart@epa.gov and requesting a 
  'QAD User ID'. See 
  \href{http://www.epa.gov/airdata/tas_Data_Mart_Registration.html}{this page} 
  for more details.

}

\seealso{
  \code{\link{getParameters}}
}
\examples{

  # Set the default username and password for a query.
  setDefaults(list(user = "your_username", pw = "your_password"))
  
  # View default parameters
  getDefaults()
  
  # Clear the username and password defaults
  clearDefaults()
  
  #View default parameters to see they have been reset
  getDefaults()

}
