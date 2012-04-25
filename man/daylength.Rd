\name{daylength}
\alias{daylength}
\title{Calculate Length of Day}
\description{Compute duration of day light for a given latitude and julian day.}
\usage{
daylength(lat, long, jd, tmz)
}

\arguments{
  \item{lat}{Latitude in degrees and decimal fraction.}
  \item{long}{Longitude in degrees and decimal fraction.}
  \item{jd}{Julian Day}
  \item{tmz}{Timezone, west of Greenwich is negative.}
}
\details{It considers sunrise and sunset of the center of the sun, it does not take into account limb, summer time, atmospheric refraction or twilight.}
\value{
\item{sunrise }{Time of sunrise.}
\item{sunset }{Time of sunset.}
\item{daylen }{Duration of daylight in hours and decimal fraction}
It returns NA for sunrise and sunset during the polar night.
 }
\references{
Corripio, J. G.: 2003, Vectorial algebra algorithms for calculating terrain parameters from DEMs
and the position of the sun for solar radiation modelling in mountainous terrain, \emph{International
Journal of Geographical Information Science} 17(1), 1-23.
}
\author{Javier G. Corripio}
\note{
You may like to double check at: \url{http://aa.usno.navy.mil/data/}
}

\seealso{\code{\link{declination}}, \code{\link{eqtime}} }
\examples{
daylength(47,11,JDymd(2012,1,1,12),1)
daylength(c(47,75),11,2456282,1)

# Daylength for the whole 2012 year
jd2012=JD(seq(ISOdate(2012,1,1),ISOdate(2012,12,31),by='day'))
plot(daylength(47,11,jd2012,1)[,3],xlab='Day of the year',ylab='day length [h]')

}
