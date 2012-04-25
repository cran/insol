\name{p2rho}
\alias{p2rho}
\title{
Air Pressure to Density
}
\description{
Calculates air density for a given pressure, temperature and relative humidity.
}
\usage{
p2rho(Pz, TempK, RH)
}

\arguments{
  \item{Pz}{
Air pressure in hPa
}
  \item{TempK}{
Air temperature in K
}
  \item{RH}{
Relative humidity (\%)
}
}

\value{
Air density (kgm^-3)
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

p2rho(1013, 288, 60)

# plot density vertical profile

z=seq(0, 10000,100)
press=z2p(z)
Tair=288-0.0065*z
plot(z,p2rho(press,Tair,50))

}
