\name{sunvector}
\alias{sunvector}

\title{
Solar Vector
}
\description{
Calculates a unit vector in the direction of the sun from the observer position.
}
\usage{
sunvector(jd, latitude, longitude, timezone)
}

\arguments{
  \item{jd}{
Julian day and decimal fraction.
}
  \item{latitude}{
Latitude of observer in degrees and decimal fraction.
}
  \item{longitude}{
Longitude of observer in degrees and decimal fraction.
}
  \item{timezone}{
Time zone, west is negative.
}
}
\details{
 To calculate the sunvector to the nearest hour, give the Julian day with a precission 
 better than 1/24; to approximate it to the nearest minute use decimal fractions smaller than 1/(24*60), and so on.

}
\value{
3 column matrix with the x, y , z coordinates of the sun vector.
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
\code{\link{sunpos}}
}
\examples{
# Current solar vector at Greenwich observatory
sunvector(JD(Sys.time()),51.4778,-0.0017,0)

juneday=JD(seq(ISOdate(2012,6,21,0),ISOdate(2012,6,21,23,30),by='30 min'))
\dontrun{ 
# Path of the sun over Greenwich in summer
require(scatterplot3d)
scatterplot3d(sunvector(juneday,51.4778,-0.0017,0),
	ylim=c(-1,1),zlim=c(0,1),pch=8,color='orange')
}	
# print values 
options(digits=12)		# make sure decimals are printed
sunvector(juneday,51.4778,-0.0017,0)

}
