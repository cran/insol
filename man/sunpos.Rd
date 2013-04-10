\name{sunpos}
\alias{sunpos}
\title{
Azimuth and zenith of the Sun  
}
\description{
Returns a matrix of azimuth and zenith angles of the sun given the unit vectors from the observer to the direction of the sun. 
}
\usage{
sunpos(sunv)
}

\arguments{
  \item{sunv}{
coordinates x, y, z of the unit vector in the direction of the sun.
}
}

\value{
A matrix of azimuth and zenith angles.
}
\references{
Corripio, J. G.: 2003, Vectorial algebra algorithms for calculating terrain parameters from DEMs
and the position of the sun for solar radiation modelling in mountainous terrain, \emph{International
Journal of Geographical Information Science} 17(1), 1-23.
}
\author{
Javier G. Corripio
}

\seealso{
\code{\link{sunvector}},
}
\examples{
## Julian Day hourly intervals at spring equinox
jd=JD(seq(ISOdate(2012,3,20,0),ISOdate(2012,3,20,23),by="hour"))

## sun position
sp=sunpos(sunvector(jd,46.813,9.844,1))

## daylight zenith<=90
sp=sp[which(sp[,2]<=90),]

## Plot the apparent solar path at Davos on the spring equinox
ramp = colorRamp(c("red", "orange","yellow"))
crmp=c(rgb(ramp(seq(1/6,1,1/6)), max = 255),rgb(ramp(seq(1,1/6,-1/6)), max = 255))
plot(sp[,1],90-sp[,2],xlab='Azimuth',
	ylab='Elevation',main='Apparent solar path at Davos on the spring equinox',
	pch=20,col=crmp,cex=(300-sp[,2])/90)
	
\dontrun{
require(plotrix)
polar.plot(90-sp[,2],sp[,1],start=90,clockwise=TRUE,rp.type='s',
	point.symbols=20,point.col=2,cex=2,radial.lim=c(0,90),
	main='Apparent solar path at Davos on the spring equinox')
}
}
