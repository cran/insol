\name{wvapsat}
\alias{wvapsat}
\title{
Saturation pressure of water vapor
}
\description{
Computes the saturation pressure of water vapour in air over water or ice.
}
\usage{
wvapsat(tempk, ice)
}
\arguments{
  \item{tempk}{
Air temperature [K].
}
  \item{ice}{
Over water or ice [0,1].
}
}

\value{
Partial pressure of water vapour [hPa].
}
\references{
Lowe, P. R.: 1977, An approximating polynomial for the computation of saturation vapor pressure,
\emph{Journal of Applied Meteorology} 16, 100-103.
}
\author{
Javier G. Corripio
}

\examples{
## Plot the differences saturation pressure over water and over ice
plot(wvapsat(250:300), xlab='Temperature', ylab='saturation vapour pressure [hPa]')

Tair = 223:273
plot(Tair,wvapsat(Tair),ty='l',lwd=2,col=4,xlab='Temperature', 
	ylab='saturation vapour pressure [hPa]')
lines(Tair,wvapsat(Tair,ice=1),col=8)
legend('topleft',c('saturation pressure over water','saturation pressure over ice'),
col=c(4,8),lwd=2)

}
