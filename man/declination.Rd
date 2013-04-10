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
\url{http://www.esrl.noaa.gov/gmd/grad/solcalc/calcdetails.html}

Meeus, J. 1999.  \emph{Astronomical Algorithms}. Willmann-Bell, Richmond, Virginia, USA.

Reda, I. and Andreas, A. 2003. \emph{Solar Position Algorithm for Solar Radiation Applications}. 55 pp.; NREL Report No. TP-560-34302, Revised January 2008. 
\url{http://www.nrel.gov/docs/fy08osti/34302.pdf}
}

\author{ Javier G. Corripio}

\examples{

declination(JDymd(2012,1,1))

jdays=JD(ISOdate(2012,1:12,21))
declination(jdays)

## Find the solstices to the nearest hour
jdays=JD(seq(ISOdate(2013,1,1),ISOdate(2013,12,31),by='hour'))
decl=declination(jdays)
wintersolstice=which(decl==min(decl))
summersolstice=which(decl==max(decl))
JD(jdays[c(summersolstice,wintersolstice)],inv=TRUE)


## Plot daily changes in declination from 2012 to 2014
jdays=JD(seq(ISOdate(2012,1,1),ISOdate(2014,12,31),by='day'))
plot(declination(jdays),xlab='days from 2012-01-01',ylab='declination')


}
