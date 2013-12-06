\name{importAQDM}
\alias{importAQDM}
\title{
Retrieve Data from U.S. EPA's Air Quality Data Mart (AQDM)
}
\description{
Performs a synchronous (rawData) data request or completes an asynchronous (rawDataNotify) data request from AQDM.
}
\usage{
importAQDM(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
either a list of parameters to be sent to AQDM or a \code{aqdmrequest} object produced by \code{requestAQDM}.
}
}
\details{
When \code{x} is missing, the function assumes the user wants to perform a synchronous data request of AQDM and calls \code{getParameters} to create a parameter list for the query. \cr\cr

Alternatively, when \code{x} is a \code{aqdmrequest} object, the function will attempt to retrieve the data resulting from the asynchronous data request that produced the \code{aqdmrequest} object.\cr\cr

The \code{x} argument can be built without use of the \code{getParameters} function. It should be a list with the named elements corresponding to the values described in U.S. EPA's \href{http://www.epa.gov/airdata/tas_URL_Query_Construction_Details_rawData.html}{Query Air Data (QAD) User's Guide}. The class of the list also need to be changed to \code{aqdmparams}. When using this method, it is left to the user to ensure that the parameter list being provided meets with EPA's requirements.
}
\value{
  an object of class \code{AQDM}. In reality this is just a dataframe with it's class set to \code{AQDM}. In most cases the object will react as a \code{data.frame} but this leaves the possibility open to write custom methods for the data in the future.
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
  \code{\link{getParameters}}
}

\examples{
  \dontrun{x <- importAQDM()}
}