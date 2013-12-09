\name{raqdm-package}
\alias{raqdm-package}
\alias{raqdm}
\docType{package}
\title{
Access to U.S. EPA's Air Quality Data Mart
}
\description{
Provides a direct interface to U.S. EPA's Air Quality Data Mart through use of its Query Air Data (QAD) web query interface. Allows for manual data queries as well as GUI-directed data queries.
}
\details{
  \tabular{ll}{
    Package: \tab raqdm\cr
    Type: \tab Package\cr
    Version: \tab 0.5\cr
    Date: \tab 2013-12-09\cr
    License: \tab GPL 2\cr
  }
}
\note{

  Accessing data from EPA's AQDM requires a username and password. Anyone can 
  get a username and password by emailing aqsdatamart@epa.gov and requesting a 
  'QAD User ID'. See 
  \href{http://www.epa.gov/airdata/tas_Data_Mart_Registration.html}{this page} 
  for more details.

}

\author{
Eric Bailey \cr
Maintainer: Eric Bailey <ebailey@idem.in.gov>
}
\references{
\href{http://www.epa.gov/airdata/toc.html}{Query Air Data (QAD) User's Guide}
}
\keyword{ package }
\examples{
  ## Make an asynchronous (rawDataNotify) request to the AQDM
  \dontrun{x <- requestAQDM()}
  
  ## Retrieve results of asynchronous request
  \dontrun{y <- importAQDM(x)}
  
  ## Make a synchronous (rawData) request to the AQDM
  \dontrun{x <- importAQDM()}
  
  ## Set the default state parameter to 18 (Indiana) and the county to 089 (Lake).
  \dontrun{setDefault(list(state="18", county="089"))}
}