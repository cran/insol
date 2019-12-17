\name{slope}
\alias{slope}

\title{ Slope of grid cells in a DEM }
\description{
Calculates the slope of every grid cell in a digital elevation model (DEM) from 
the output of cgrad, which is a set of unit vectors normal to every grid cell.
}
\usage{
slope(cgrad, degrees = FALSE)
}
\arguments{
  \item{cgrad}{
 A 3D array of dimensions nrow(dem), ncol(dem),3, where the third dimensions are 
 the x, y z component of the unit vectors normal to the surface of the DEM grid cells. }
  \item{degrees}{
Logical. If \code{FALSE}, returns radians, if \code{TRUE}, returns degreees. }
}

\value{
A matrix of slope values for all grid cells.
}

\seealso{
\code{\link{aspect}}, \code{\link{cgrad}}
}
\examples{
# Calculate the slope of a rough mountain area in the pyrinees
zipfile = tempfile()
download.file("https://meteoexploration.com/R/insol/data/dempyrenees.asc.zip",zipfile)
header = read.table(unz(zipfile,'dempyrenees.asc'),nrows=6)
dem = read.table(unz(zipfile,'dempyrenees.asc'),skip=6)
dem = as.matrix(dem)
unlink(zipfile)
cellsize = header[5,2]
slopedem = slope(cgrad(dem,cellsize),degrees=TRUE)
image(t(slopedem[nrow(slopedem):1,]),col=grey(100:1/100))


## similar but using raster
\dontrun{
require(raster)
demfile = tempfile()
download.file("https://meteoexploration.com/R/insol/data/dempyrenees.tif",demfile)
dem = raster(demfile)
slopedem = slope(cgrad(dem),degrees=TRUE)
slopedem = raster(slopedem,crs=projection(dem))
extent(slopedem) = extent(dem)
plot(slopedem,col = grey(100:1/100))
unlink(demfile)
}
}
