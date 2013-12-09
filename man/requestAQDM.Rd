\name{requestAQDM}
\alias{requestAQDM}

\title{
Initiate Asynchronous Data Retrieval from U.S. EPA's Air Quality Data Mart (AQDM)
}
\description{
  Sends a 'rawDataNotify' request URL to U.S. EPA's Air Quality Data Mart and stores the retrieval code so data can be easily imported into R later using \code{importAQDM}. Using 'rawDataNotify' can allow for larger data pulls and you can continue using R while the data is being retrieved.
}
\usage{
requestAQDM(x)
}

\arguments{
  \item{x}{a list of query parameters to include in the data request}
}
\details{
  If \code{x} is missing the function will automatically call \code{getParameters} which will open a \code{tcltk} dialog to assist in building the parameter list. See \code{\link{getParameters}} for more details.\cr\cr
  
The \code{x} argument can be built without use of the \code{getParameters} function. It should be a list with the named elements corresponding to the values described in U.S. EPA's \href{http://www.epa.gov/airdata/tas_URL_Query_Construction_Details_rawData.html}{Query Air Data (QAD) User's Guide}. The class of the list also need to be changed to \code{aqdmparams}. When using this method, it is left to the user to ensure that the parameter list being provided meets with U.S. EPA's requirements.
}

\value{
  an object of class \code{aqdmrequest}. When the request is complete this object can be provided to the \code{importAQDM} function to retieve the requested data.
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
\code{\link{importAQDM}} \cr
\code{\link{getParameters}}
}
\examples{
  \dontrun{x <- requestAQDM()}
}

