\name{JD}
\alias{JD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Julian Day
}
\description{
Computes Julian Day from POSIXct object
}
\usage{
JD(x)
}

\arguments{
  \item{x}{
a POSIXct object
}
}
\details{
Class "POSIXct" represents the (signed) number of seconds since the beginning of 1970 (in the UTC timezone) as a numeric vector, and Julian Day is the number of days since January 1, 4713 BCE at noon UTC, so the Julian day is calculated as numeric(POSIXct)+2440587.5 days. 
}
\value{
Julian Day
}

\author{
Javier G. Corripio
}
\note{
You may like to double check the results here: \url{http://aa.usno.navy.mil/data/docs/JulianDate.php}
}

\seealso{
\code{\link{JDymd}}
}
\examples{
JD(Sys.time())
	
JD(seq(ISOdate(2012,1,21),ISOdate(2012,12,21),by='month'))
}

