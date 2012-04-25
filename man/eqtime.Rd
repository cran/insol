\name{eqtime}
\alias{eqtime}

\title{ Equation of Time }
\description{ Computes the equation of time for a given julian day. }
\usage{
eqtime(jd)
}

\arguments{
  \item{jd}{Julian Day}
}

\value{Equation of time in minutes }

\references{
\url{http://www.esrl.noaa.gov/gmd/grad/solcalc/calcdetails.html}

Meeus, J. 1999.  \emph{Astronomical Algorithms}. Willmann-Bell, Richmond, Virginia, USA.

Reda, I. and Andreas, A. 2003. \emph{Solar Position Algorithm for Solar Radiation Applications}. 55 pp.; NREL Report No. TP-560-34302, Revised January 2008. 
\url{http://www.nrel.gov/docs/fy08osti/34302.pdf}
}
\author{Javier G. Corripio}


\examples{
# plot the equation of time for 2012 at daily intervals
tsx=seq(ISOdate(2012,1,1),ISOdate(2012,12,31),by='day')
jd=JD(tsx)
plot(eqtime(jd))
abline(h=0,col=8)

# Analema
plot(eqtime(jd),declination(jd))

# Analema from Greenwich Observatory
x = eqtime(jd)*15/60
y = 90-51.4791+declination(jd)
plot(x,y,xlab=expression(paste('Azimuth (',degree,')')),
	ylab=expression(paste('Elevation (',degree,')')))

}

