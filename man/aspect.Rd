\name{aspect}
\alias{aspect}
\title{ Aspect or orientation of the slope }
\description{
Calculates the aspect of every grid cell in a digital elevation model (DEM) from 
the output of cellgradient, which is a set of unit vectors normal to every grid cell in the DEM. }
\usage{
aspect(cgrad, degrees = FALSE)
}

\arguments{
  \item{cgrad}{
 A 3D array of dimensions [\code{nrow(dem)}, \code{ncol(dem)}, 3], where the third dimensions are 
 the x, y, z component of the unit vectors normal to the surface of the DEM grid cells.
}
  \item{degrees}{
Logical. If \code{FALSE}, returns radians, if \code{TRUE}, returns degreees.
}
}
\details{
Uses \code{atan2()} to compute the orientation within the range \eqn{[0, 2\pi]}
}
\value{
Aspect or orientation of the slope.
}

\seealso{
\code{\link{slope}}, \code{\link{cgrad}}
}
\examples{
# Create a west-east facing ramp
slpwe = rep(1,10)  \%o\% c(1:5,4:1)
# calculate the aspect at every node or grid cell (it should be 270 or 90 degrees):
cgr = cgrad(slpwe,1)
aspect(cgr,degrees=TRUE)

# Calculate the aspect of a rough mountain area in the pyrinees
zipfile = tempfile()
download.file("https://meteoexploration.com/R/insol/data/dempyrenees.asc.zip",zipfile)
header = read.table(unz(zipfile,'dempyrenees.asc'),nrows=6)
dem = read.table(unz(zipfile,'dempyrenees.asc'),skip=6)
dem = as.matrix(dem)
unlink(zipfile)
cellsize = header[5,2]
aspectdem = aspect(cgrad(dem,cellsize),degrees=TRUE)
image(t(aspectdem[nrow(aspectdem):1,]),col=grey(1:100/100))


\dontrun{ ## raster package display nicer than image and handles projections:
require(raster)
demfile = tempfile()
download.file("https://meteoexploration.com/R/insol/data/dempyrenees.tif",demfile)
dem = raster(demfile)
aspectdem = aspect(cgrad(dem),degrees=TRUE)
aspectdem = raster(aspectdem,crs=projection(dem))
extent(aspectdem) = extent(dem)
plot(aspectdem,col=grey(1:100/100))
unlink(demfile)
}
}
