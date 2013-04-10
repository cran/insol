\name{daydoy}
\alias{daydoy}
\alias{daydoy,missing-method}
\alias{daydoy,numeric-method}
\alias{daydoy,POSIXct-method}

\title{Dates to day of the year}
\description{Returns day of the year for given dates.}
\usage{

\S4method{daydoy}{numeric}(x,month,day)
%%daydoy(x, month, day)

\S4method{daydoy}{POSIXct}(x) 
%%daydoy(x)
}

\arguments{
  \item{x}{ Year, four digits format, or an object of class POSIXct with no extra arguments}
  \item{month}{Month number.}
  \item{day}{Day of the month.}
}
\value{
Day of the year [1:366].
}

\seealso{
\code{\link{ISOdate}}
}
\examples{
daydoy(2012,2,27:29)
daydoy(ISOdate(2012,2,27:29))


}
