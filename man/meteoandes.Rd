\name{meteoandes}
\alias{meteoandes}
\docType{data}
\title{
Mountain meteorological data
}
\description{
Meteorological data from an automatic weather station in the Central Andes of Chile.
}
\usage{data(meteoandes)}
\format{
  A data frame with 1152 observations on the following 10 variables.
  \describe{
    \item{\code{year}}{Year}
    \item{\code{doy}}{Day of the year}
    \item{\code{hh}}{hour}
    \item{\code{mm}}{minute}
    \item{\code{Tair}}{Air temperature, grades centigrade}
    \item{\code{pyra1}}{Incoming solar short-wave radiation Wm^-2}
    \item{\code{pyra2}}{Reflected solar short-wave radiation Wm^-2 }
    \item{\code{windspeed}}{Wind speed, ms^-1}
    \item{\code{winddir}}{Wind direction, degrees}
    \item{\code{RH}}{Relative humidity \%}
  }
}

\source{
Measured by the author on Loma Larga Glacier, -33.6917, -70.0, 4640 m a.s.l. January 2001.
}
\references{

Corripio, J. G. and Purves, R. S.: 2005, Surface energy balance of high altitude glaciers 
in the Central Andes: the effect of snow penitentes, in C. de Jong, D. Collins and 
R. Ranzi (eds), \emph{Climate and Hydrology in Mountain Areas}, Wiley, London, chapter 3, pp. 15-27.
}
\examples{
data(meteoandes)
str(meteoandes) 

# plot the 2 pyranometers measurements 
# one facing up: incident insolation, one facing down: reflected insolation

meteodate = strptime(paste(meteoandes$year,meteoandes$doy,meteoandes$hh,meteoandes$mm),
	format="\%Y \%j \%H \%M",tz="America/Santiago")
plot(meteodate,meteoandes$pyra1,'l',col=2,xlab='Date',
	ylab=expression(paste('Solar radiation [ ',Wm^-2,' ]')),
	main='Insolation at Loma Larga glacier') 
lines(meteodate,meteoandes$pyra2,col=4)

}
\keyword{datasets}
