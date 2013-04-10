\name{GCdistance}
\alias{GCdistance}
\title{Great circle distance}
\description{Great circle or geodesic distance. }
\usage{
GCdistance(lat1,lon1,lat2,lon2)
}
\arguments{
  \item{lat1}{Latitude of points of origin. }
  \item{lon1}{Longitude of points of origin. }
  \item{lat2}{Latitude of points of destination. }
  \item{lon2}{Longitude of points of destination. }
}
\value{
Distance between origin and destination points in metres.
}
\references{
\url{http://williams.best.vwh.net/avform.htm#Dist}, 

\url{http://mathworld.wolfram.com/GreatCircle.html}.
}
\author{
Javier G. Corripio
}

\examples{
GCdistance(0,0,0,180)*2

# distance between the center of US states 
data(state)
ddd=matrix(nrow=50,ncol=50,dimnames=list(state.name,state.name))
for (i in 1:50){
	for (j in 1:50){
		distij=GCdistance(state.center$y[i],state.center$x[i],
			state.center$y[j],state.center$x[j])
		# round to miles
		ddd[i,j]=round(distij/1609.344,2)
	}
}
# format and print results for the 10 firsts states
as.dist(ddd[1:10,1:10])


}