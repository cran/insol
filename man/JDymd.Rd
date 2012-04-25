\name{JDymd}
\alias{JDymd}
\title{
Julian Day
}
\description{
Computes Julian Day from a given date
}
\usage{
JDymd(year,month,day,hour=12,minute=0,sec=0,tmz=0)
}

\arguments{
  \item{year}{
numeric year
}
  \item{month}{
1-12: number of the month. 
}
  \item{day}{
1-31: day of the month.
}
  \item{hour}{
0-23: hour of the day.
}
  \item{minute}{
0-59: minutes.
}
  \item{sec}{
0-59: seconds.
}
  \item{tmz}{
time zone in hours, west is negative.
}
}
\value{
Julian Day, or number of days since January 1, 4713 BCE at noon UTC
}
\references{
Danby, J. M. Eqn. 6.16.4 in \emph{Fundamentals of Celestial Mechanics}, 2nd ed. Richmond, VA: Willmann-Bell, p. 207, 1988.

\url{http://scienceworld.wolfram.com/astronomy/JulianDate.html}
}
\author{
Javier G. Corripio
}
\note{
You may double like to double check the results here: \url{http://aa.usno.navy.mil/data/docs/JulianDate.php}
}

\section{Warning }{This simplification is only valid between 1901 and 2099.} 

\seealso{
\code{\link{JD}}
}
\examples{
JDymd(2012,3,20,12)

print(paste('Number of days since the beginning of the century (1/1/2001):',
	JD(Sys.time())-JDymd(2001,1,1,0)))
}
