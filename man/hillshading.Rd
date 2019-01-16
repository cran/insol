\name{hillshading}
\alias{hillshading}
\title{ 
Intensity of illumination over a surface
}
\description{
Computes the intensity of illumination over a surface, such as a DEM, according to the position of the sun.
}
\usage{
hillshading(cgrad, sv)
}
\arguments{
  \item{cgrad}{
3D array ( nrow, ncol, 3) of unit vectors normal to surface grid cells. The output of \code{cgrad()}.}
  \item{sv}{
unit vector in the direction of the sun.}
}
\details{
The intensity of the sun beams falling on a surface are proportional to the cosine of 
the angle between the sun vector and the vector normal to the surface, which in this case 
is the dot product  between \code{cgrad} and  \code{sv}. }
\value{
A matrix of illumination intensity values. 
Negative values are the equivalent of self shading and are set to zero. }
\references{
Corripio, J. G.: 2003, Vectorial algebra algorithms for calculating terrain parameters from DEMs
and the position of the sun for solar radiation modelling in mountainous terrain, \emph{International
Journal of Geographical Information Science} 17(1), 1-23.
}
\author{
Javier G. Corripio
}
\seealso{
\code{\link{cgrad}, \link{insolation}, \link{sunvector}}
}
\examples{
\dontrun{
## From the "Obligatory Mathematical surface" in persp3d {rgl}
## this shows the cast shdows clearly, but there is some interference with rgl internal
## lit parameters
require(rgl)
x = seq(-10, 10, length= 300)
y = x
f = function(x,y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
z = outer(x, y, f)
z[is.na(z)] = 1
zgr = cgrad(z,2/30)
sv = normalvector(55,315)
hsh = zgr[,,1]*sv[1]+zgr[,,2]*sv[2]+zgr[,,3]*sv[3]
hsh = (hsh+abs(hsh))/2
sh = doshade(z,sv,2/30)
hshsh = hsh*sh
hshsh = t(hshsh[nrow(hshsh):1,])
open3d()
rgl.light(viewpoint.rel = F, ambient = "#FFFFFF", diffuse = "#FFFFFF", specular = "#000000")
persp3d(x, y, z, col=grey(hshsh))
}

## Hillshading on mountain terrain, sun at NW, 35 degrees elevation
zipfile = tempfile()
download.file("https://meteoexploration.com/R/insol/data/dempyrenees.asc.zip",zipfile)
header = read.table(unz(zipfile,'dempyrenees.asc'),nrows=6)
dem = read.table(unz(zipfile,'dempyrenees.asc'),skip=6)
dem = as.matrix(dem)
unlink(zipfile)
cellsize = header[5,2]
sv = normalvector(55,315)
grd = cgrad(dem,cellsize)
hsh = grd[,,1]*sv[1]+grd[,,2]*sv[2]+grd[,,3]*sv[3]
## remove negative incidence angles (self shading) 
hsh = (hsh+abs(hsh))/2
sh = doshade(dem,sv,cellsize)
hshsh = hsh*sh
image(t(hshsh[nrow(sh):1,]),col=grey(1:100/100))

\dontrun{
## Hillshading on mountain terrain, sun at NW, 45 degrees elevation. Using raster
sv = normalvector(45,315)
require(rgdal)
require(raster)
demfile = tempfile()
download.file("https://meteoexploration.com/R/insol/data/dempyrenees.tif",demfile)
dem = raster(demfile)
grd = cgrad(dem)
hsh = grd[,,1]*sv[1]+grd[,,2]*sv[2]+grd[,,3]*sv[3]
## remove negative incidence angles (self shading) 
hsh = (hsh+abs(hsh))/2
hsh = raster(hsh,crs=projection(dem))
extent(hsh) = extent(dem)
plot(hsh,col=grey(1:100/100),legend=FALSE)
unlink(demfile)
}

}