\name{rh2sh}
\alias{rh2sh}

\title{
Relative Humidity to Specific Humidity
}
\description{
Computes specific humidity from given relative humidity, temperature and pressure.
}
\usage{
rh2sh(RH, tempk, Pz, ice)
}

\arguments{
  \item{RH}{
Relative humidity (\%).
}
  \item{tempk}{
Air temperature in K
}
  \item{Pz}{
Air pressure in hPa
}
  \item{ice}{
Whether over water or ice surface (0,1).
}
}

\value{
Specific humidity (kg/kg).
}
\references{
Brutsaert, W.: 1982, \emph{Evaporation into the atmosphere : theory, history, and applications}, Reidel,
Dordrecht. 1984 edition.
}
\author{
Javier G. Corripio
}

\seealso{
\code{\link{wvapsat}}
}
\examples{

plot(250:300-273.15,rh2sh(50, 250:300, 1013, 0),xlab='Temperature [C]', 
ylab='specific humidity',
main='Specific humidity for RH=0.5 and varying temperature')

}
