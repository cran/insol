\name{cgrad}
\alias{cgrad}
\title{ Normal vector to every grid cell in a DEM
}
\description{
Computes a unit vector normal to every grid cell in a digital elevation model.
}
\usage{
cgrad(dem, dlx, dly = dlx, cArea = FALSE)
}
\arguments{
  \item{dem}{
Digital Elevation Model, either a matrix or a Raster Layer.
}
  \item{dlx}{
Resolution along X axis (longitude).
}
  \item{dly}{
Resolution along Y axis (latitude).
}
  \item{cArea}{
Logical, if \code{TRUE} returns the surface area of every grid cell instead of the gradient.
}
}
\details{
The normal vector to the grid cell is the cross product of vectors along the sides of the 
polygon that form the grid cell. 
By definition the length of this vector is equal to the area of the polygon.
}

\value{
Returns a 3D matrix with the 2 first dimensions as input dem 
and the 3rd dimension of value 3 corresponding to the x, y , z coordinates 
of a unit vector perpendicular to every grid cell.
If cArea is \code{TRUE}, the result is a 2D matrix with the surface area of every grid cell.
}
\references{
Corripio, J. G.: 2003, Vectorial algebra algorithms for calculating terrain parameters from DEMs
and the position of the sun for solar radiation modelling in mountainous terrain, \emph{International
Journal of Geographical Information Science} 17(1), 1-23.
}
\author{
Javier G. Corripio
}
\note{
The returned information for every cell is contained by the node at the upperleft corner 
and the last column and row are undefined. The values given for the last colum and row 
are a replication of the previous column and row.
}

\section{warning}{
\code{dlx} ad \code{dly} are assumed to be constant over the extension of the DEM, 
therefore the projection should not be \code{latlong}. 
In this case the resolution is a constant angle, and the equivalent distance 
on the surface changes with latitude, giving incorrrect results. 
}

\seealso{
\code{\link{aspect}},  \code{\link{slope}},
}
\examples{
## visualize x, y z components of vector normal to a DEM representing a regular pyramid
m = matrix(0,nrow=100,ncol=100)
for (i in 1:100){ for (j in 1:100){
m[i,j]=50-max(abs(i-50),abs(j-50)) }}
grdm = cgrad(m,1)
xcomponent = grdm[,,1]
ycomponent = grdm[,,2]
zcomponent = grdm[,,3]
image(t(xcomponent[nrow(xcomponent):1,]) ,col=grey(1:10/10))
image(t(ycomponent[nrow(ycomponent):1,]) ,col=grey(1:10/10))
image(t(zcomponent[nrow(zcomponent):1,]) ,col=grey(1:10/10))


## Surface area of every grid cell in a mountain region
## Steep slopes correspond to larger surface area per grid cell
zipfile = tempfile()
download.file("https://meteoexploration.com/R/insol/data/dempyrenees.asc.zip",zipfile)
header = read.table(unz(zipfile,'dempyrenees.asc'),nrows=6)
dem = read.table(unz(zipfile,'dempyrenees.asc'),skip=6)
dem = as.matrix(dem)
unlink(zipfile)
cellsize = header[5,2]
grdarea = cgrad(dem,cellsize,cArea=TRUE)
image(t(grdarea[nrow(grdarea):1,]),col=grey(100:1/100))

## plot the relationship between surface slope and surface area per grid cell:
slpg = slope(cgrad(dem,cellsize),degrees=TRUE)
plot(slpg,grdarea)

## This would be a linear relationship in 2D, 
## but in a DEM the slope of the grid cell depends on 4 points in 3D
plot(tan(radians(slpg)),grdarea)

\dontrun{
## Use raster for better display and keep the dem projection
require(raster)
demfile = tempfile()
download.file("https://meteoexploration.com/R/insol/data/dempyrenees.tif",demfile)
dem = raster(demfile)
plot(dem)
grd = cgrad(dem)
grdarea = cgrad(dem,cArea=TRUE)
rgrdarea = raster(grdarea,crs=projection(dem))
extent(rgrdarea) = extent(dem)
plot(rgrdarea,col = grey(100:1/100))
contour(dem,col='sienna1',lwd=.5,nlevels=30,add=TRUE)
unlink(demfile)
}

}
