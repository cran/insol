\name{z2p}
\alias{z2p}

\title{
Altitude to Pressure}
\description{
Computes air pressure for a given altitude according to the standard atmosphere.
}
\usage{
z2p(z, P0 = 101325, T0 = 288.15)
}

\arguments{
  \item{z}{
altitude above sea level in metres [0:10000].
}
  \item{P0}{
Pressure at sea level.
}
  \item{T0}{
Temperature at sea level.
}
}

\value{
Pressure in hPa.}
\references{
U.S. NOAA: 1976, \emph{U.S. standard atmosphere, 1976}, NOAA-S/T; 76-1562, U.S. National Oceanic
and Atmospheric Administration, National Aeronautics and Space Administration, United States
Air Force, Washington. 227 pp.
}
\author{
Javier G. Corripio
}

\examples{

# Plot pressure form sea level to the top of Mt. Everest
plot(z<-0:8848,z2p(z),'l',xlab='Altitude [m]',ylab='Pressure [hPa]')

}
