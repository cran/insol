\name{declination}
\alias{declination}
\title{Declination}
\description{Computes the declination of the Sun for a given Julian Day.}
\usage{
declination(jd)
}
\arguments{
  \item{jd}{Julian Day.}
}

\value{ Declination in degrees and decimal fraction. }

\references{
\url{https://www.esrl.noaa.gov/gmd/grad/solcalc/calcdetails.html}

Meeus, J. 1999.  \emph{Astronomical Algorithms}. Willmann-Bell, Richmond, Virginia, USA.

Reda, I. and Andreas, A. 2003. \emph{Solar Position Algorithm for Solar Radiation Applications}. 55 pp.; NREL Report No. TP-560-34302, Revised January 2008. 
\url{https://www.nrel.gov/docs/fy08osti/34302.pdf}
}

\author{ Javier G. Corripio}

\examples{

declination(JDymd(2019,1,1))

jdays = JD(ISOdate(2019,1:12,21))
declination(jdays)

## Plot daily changes in declination from 2018 to 2020
jdays=JD(seq(ISOdate(2018,1,1),ISOdate(2020,12,31),by='day'))
plot(declination(jdays),xlab='days from 2018-01-01',ylab='declination')


}
