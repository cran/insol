\name{sunr}
\alias{sunr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Earth Radius Vector
}
\description{
Calculates the Earth radius vector.
}
\usage{
sunr(jd)
}

\arguments{
  \item{jd}{
Julian Day
}
}

\value{
Earth Radius Vector in Astronomical Units (AU). This is used to modify the solar constant as a function of the earth-sun distance.}
\references{
\url{http://www.esrl.noaa.gov/gmd/grad/solcalc/calcdetails.html}

Meeus, J. 1999.  \emph{Astronomical Algorithms}. Willmann-Bell, Richmond, Virginia, USA.

Reda, I. and Andreas, A. 2003. \emph{Solar Position Algorithm for Solar Radiation Applications}. 55 pp.; NREL Report No. TP-560-34302, Revised January 2008. 
\url{http://www.nrel.gov/docs/fy08osti/34302.pdf}

}
\author{
Javier G. Corripio
}

\examples{

# plot the variation of the earth radius vector over the next year
days_nexty=seq(Sys.time(),Sys.time()+86400*365,by='day')
plot(days_nexty,sunr(JD(days_nexty)),xlab='Date',ylab='Earth Radius Vector [AU]')
abline(h=1,col=8)
}
