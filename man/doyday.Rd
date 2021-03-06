\name{doyday}
\alias{doyday}
\title{Day of the year to date}
\description{Returns the date for given days of the year.}
\usage{
doyday(year, doy)
}

\arguments{
  \item{year}{Year, four digits format. It can have a decimal fraction if day is omitted.}
  \item{doy}{Day of the year [1:366].}
}
\value{
returns an object of class \code{POSIXlt}.
}

\author{ Javier G. Corripio }

\seealso{
\code{\link{as.POSIXlt}}
}
\examples{
doyday(2019,58:65)

doyday(2019.5)


}