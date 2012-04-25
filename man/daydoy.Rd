\name{daydoy}
\alias{daydoy}
\title{Dates to day of the year}
\description{Returns day of the year for given dates.}
\usage{
daydoy(year, month, day)
}

\arguments{
  \item{year}{Year, four digits format.}
  \item{month}{Month number.}
  \item{day}{Day of the month.}
}
\value{
day of the year [1:366].
}

\author{ Javier G. Corripio }

\seealso{
\code{\link{ISOdate}}
}
\examples{
daydoy(2012,2,27:29)

}
