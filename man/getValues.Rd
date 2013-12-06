\name{getValues}
\alias{getValues}
\title{
  List valid query parameter values from AQDM
}
\description{
  Returns a dataframe containing the codes and descriptions for given query parameter.
}
\usage{
getValues(name, quals, stop.on.error = TRUE)
}

\arguments{
  \item{name}{the name of the parameter being queried}
  \item{quals}{a named list of qualifying values (See details)}
  \item{stop.on.error}{a logical value indicating whether a invalid query should stop with an error}
}
\details{
  This function is primarily used internally to populate the comboboxes in the 
  \code{\link{getParameters}} function but may be useful for other purposes. \cr \cr
  In many cases you cannot query a parameter from AQDM without first narrowing
  your query. For example, you cannot query county names without first selecting
  a state. This is what the \code{params} argument is for. See examples for a 
  demonstration. \cr \cr
  If \code{stop.on.error} is \code{TRUE}(the default) providing invalid arguments
  will result in an error. If it is \code{FALSE} it will cause the function to 
  return an empty vector.
}
\value{
  A two-columns \code{data.frame} containing the code and a description for 
  each valid value for a query parameter.
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

  ## Return a data.frame of all counties in Indiana with air quality monitoring data. 
  x <- getValues(name = "county", quals = list(state="18"))
  ## Return a data.frame of all valid parameter classes.
  y <- getValues(name = "pc")

}

