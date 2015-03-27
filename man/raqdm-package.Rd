\name{raqdm-package}
\alias{raqdm-package}
\alias{raqdm}
\docType{package}
\title{
<<<<<<< HEAD
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
=======
What the package does (short line)
~~ package title ~~
}
\description{
More about what it does (maybe more than one line)
~~ A concise (1-5 lines) description of the package ~~
}
\details{
\tabular{ll}{
Package: \tab raqdm\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-02-25\cr
License: \tab What license is it under?\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Who wrote it

Maintainer: Who to complain to <yourfault@somewhere.net>
~~ The author and/or maintainer of the package ~~
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
~~ simple examples of the most important functions ~~
}
>>>>>>> b7a44f940b9f58a67c12c9c64c2d70117fc0977d
