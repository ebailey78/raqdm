\name{getParameters}
\alias{getParameters}
\title{
  Build Parameter List for U.S. EPA AQDM Data Pulls
}
\description{
  Opens a \code{tcltk} gui that allows the user to more easily build the parameter list necessary to make data requests using \code{raqdm}.
}
\usage{
  getParameters()
}
\details{
  The gui is based on the web interface provided on U.S. EPA's 
  \href{https://ofmext.epa.gov/AQDMRS/aqdmrs.html}{webpage}. \cr \cr
  In addition to the parameter input boxes there are also a set of 
  checkboxes and buttons with '?' in them. Checking a box will set the current 
  value of its corresponding input box to be the default value if the 
  \code{getParameters} function is called again in the same session. Clicking 
  the '?' box will result in a message box opening to with help about its 
  corresponding input box.
}
\value{
  an \code{aqdmparams} object containing a list of named elements that 
  correspond to the parameters of an AQDM data request.
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
  \code{\link{requestAQDM}} \cr
  \code{\link{importAQDM}}
}

\examples{

  ## Create an aqdmparams object for use with requestAQDM or importAQDM
  \dontrun{params <- getParameters()}

}