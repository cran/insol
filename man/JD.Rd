\name{JD}
\alias{JD}
\title{
Julian Day from POSIXct
}
\description{
Computes Julian Day from dates as POSIXct object.
}
\usage{
JD(x, inverse=FALSE)
}
\arguments{
  \item{x}{
POSIXct object.
}
  \item{inverse}{
Logical. If \code{false} (default) returns the Julian Days corresponding to given dates. 
If \code{TRUE} returns the date corresponding to input Julian days 
}
}
\details{
Class "POSIXct" represents the (signed) number of seconds since the beginning of 1970 
(in the UTC timezone) as a numeric vector, and Julian Day is the number of days since 
January 1, 4713 BCE at noon UTC, so the Julian Day is calculated as numeric(POSIXct)+2440587.5 days. 
}
\value{
Julian Day
}
\author{
Javier G. Corripio
}
\note{
You may like to double check the results here: \cr
\url{https://ssd.jpl.nasa.gov/tc.cgi}

To get correct values it is recommended to increase the number of digits to display:
	options(digits=12)
} 
\seealso{
\code{\link{JDymd}}
}
\examples{
JD(Sys.time())
JD(seq(ISOdate(2019,1,21),ISOdate(2019,12,21),by='month'))

}

