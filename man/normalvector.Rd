\name{normalvector}
\alias{normalvector}
\title{
Computes Vector Normal to Surface
}
\description{
Calculates a unit vector normal to a surface defined by slope inclination and slope orientation.
}
\usage{
normalvector(slope, aspect)
}
\arguments{
  \item{slope}{
slope inclination in degrees.
}
  \item{aspect}{
slope orientation in degrees.
}
}
\value{
Vector normal to surface, matrix of  {x, y, z} coordinates
}
\references{
Corripio, J. G.: 2003, Vectorial algebra algorithms for calculating terrain parameters from DEMs
and the position of the sun for solar radiation modelling in mountainous terrain, \emph{International
Journal of Geographical Information Science} 17(1), 1-23.
}
\author{
Javier G. Corripio
}

\examples{
# horizontal surface
normalvector(0,0)

# surface 45 degrees south
normalvector(45,180)

# Angle of incidence of sun on a tilted surface 15 degrees southon spring at Davos
jd=JD(seq(ISOdate(2012,3,20,0),ISOdate(2012,3,20,23),by="hour"))
degrees(acos(sunvector(jd,46.813,9.844,1) \%*\% as.vector(normalvector(15,180))))

}