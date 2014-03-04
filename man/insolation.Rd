\name{insolation}
\alias{insolation}

\title{Direct and diffuse solar radiation. }
\description{
Computes direct and diffuse solar irradiance perpendicular to the beam, for a given 
zenith angle, Julian Day, altitude and atmospheric conditions.  
}
\usage{
insolation(zenith, jd, height, visibility, RH, tempK, O3, alphag)
}

\arguments{
  \item{zenith}{ Zenith angle in degrees. }
  \item{jd}{ Julian Day. }
  \item{height}{ Altitude above sea level.}
  \item{visibility}{ Visibility [km]. }
  \item{RH}{ Relative humidity [\%]. }
  \item{tempK}{ Air temperature [K]. }
  \item{O3}{Ozone thickness [m]. }
  \item{alphag}{Albedo of the surrounding terrain [0 to 1]. }
}
\details{ See \url{http://disc.sci.gsfc.nasa.gov/Aura/data-holdings/OMI/omto3_v003.shtml} for ozone data.}
\value{
Returns a two column matrix of irradiance values.  The first column is direct radiation, the second is diffuse radiation.
}
\references{
Bird, R. E. and Hulstrom, R. L. (1981a) Review, evaluation and improvements of direct irradiance models, \emph{Trans. ASME J. Solar Energy Eng.} 103, 182-192. 

Bird, R. E. and Hulstrom, R. L. (1981b) \emph{A simplified clear sky model for direct and diffuse insolation on horizontal surfaces}, Technical Report SERI/TR-642-761, Solar Research Institute, Golden, Colorado. 

Iqbal, M. (1983) \emph{An Introduction to Solar Radiation}, Academic Press, Toronto.
}
\author{Javier G. Corripio}

\seealso{ \code{\link{doshade}}, \code{\link{hillshading}}, \code{\link{sunvector}} }

\examples{
insolation(30,2456007,3200,28,60,278.15,0.02,0.2)
insolation(30,JDymd(2012,6,21),3200,28,60,278.15,0.02,0.2)

# Compare measured and modelled insolation

# load data from automatic weather station in the Andes
data(meteoandes)

# Get zenith angle for every time step
meteodate=as.POSIXct(strptime(paste(meteoandes$year,meteoandes$doy,
	meteoandes$hh,meteoandes$mm),format="\%Y \%j \%H \%M"))
metjd=JD(meteodate)


sunv = sunvector(metjd,-33.695,-70.0033,-4) 
zenith = sunpos(sunv)[,2]

# Compute direct and diffuse beam irradiance 
Idirdif = insolation(zenith,metjd,4640,90,
	meteoandes$RH,meteoandes$Tair+273.15,0.02,0.55)

# modify for angle of incidence on horizontal surface (pyranometer)
cos_inc_sfc=sunv\%*\%as.vector(normalvector(0,0)) ## or sum(sunv*normalvector(0,0))

# set to zero values with no indicent light
cos_inc_sfc[cos_inc_sfc<0]=0

# Add direct and diffuse simulated radiation on horizontal surface
Isim  = Idirdif[,1] * cos_inc_sfc + Idirdif[,2]

# plot the measured insolation
plot(meteodate,meteoandes$pyra1,'l',col=2)

# add a shaded polygon corrresponding to 10\% accuracy in the measurements
polygon(c(meteodate, rev(meteodate)), c(meteoandes$pyra1*(1+0.1),
	rev(meteoandes$pyra1*(1-0.1))),col = "#ff000033", border = NA)

# add the simulated insolation
lines(meteodate,Isim,col=4)

# We measured that diffuse reflected solar radiation from the surrounding mountains 
# covered in snow could be up to 10\% of total incoming radiation. 
# There is one hour of shadows early in the morning (not simulated)
# Add 10\% diffuse reflected radiation
lines(meteodate,1.1*Isim,col=3)


## Calculate insolation on the island of La Palma, Spain on the 21.03.2013
## reduced resolution DEM from SRTM, http://www2.jpl.nasa.gov/srtm/
zipfile=tempfile()
download.file("http://www.meteoexploration.com/R/insol/data/demlapalma.asc.zip",zipfile)
header=read.table(unz(zipfile,'demlapalma.asc'),nrows=6)
dem = read.table(unz(zipfile,'demlapalma.asc'),skip=6)
dem=as.matrix(dem)
unlink(zipfile)
cellsize=header[5,2]
cgr=cgrad(dem,cellsize)
height=750
visibility=30
RH=80
tempK=288
tmz=0
year=2013
month=3
day=21
timeh=12
jd=JDymd(year,month,day,hour=timeh)
Iglobal=array(0,dim=dim(dem))
deltat=0.5
lat=28.135
lon=-17.247
dayl=daylength(lat,lon,jd,0)
for (srs in seq(dayl[1],dayl[2],deltat)){
	jd=JDymd(year,month,day,hour=srs)
	sv=sunvector(jd,lat,lon,tmz)
	hsh=hillshading(cgr,sv)
	sh=doshade(dem,sv,cellsize)
	zenith=sunpos(sv)[2]
	Idirdif = insolation(zenith,jd,height,visibility,RH,tempK,0.002,0.15)
	## direct radiation modified by terrain + diffuse irradiation (skyviewfactor ignored)
	## values in J/m^2
	Iglobal = Iglobal + (Idirdif[,1] * hsh + Idirdif[,2] )*3600*deltat	
}

## dispaly results
image(t(Iglobal[nrow(Iglobal):1,]),col=grey(1:100/100))
contour(t(dem[nrow(dem):1,]),lwd=.5,col='sienna1',add=TRUE,levels=seq(0,2500,500))
contour(t(dem[nrow(dem):1,]),lwd=.25,col='sienna1',add=TRUE,levels=seq(0,2500,50),drawlabels=FALSE)

\dontrun{
## The same using raster
require(rgdal)
require(raster)
demfile=tempfile()
download.file("http://www.meteoexploration.com/R/insol/data/demlapalma.tif",demfile)
dem=raster(demfile)
plot(dem)
cgr=cgrad(dem)
demm=raster:::as.matrix(dem)
dl=res(dem)[1]
## Isolation at 30 min interval over the length of the day
## RH and temp would cahnge over the dy, here we use a constant value for simplicity
height=750
visibility=30
RH=80
tempK=288
tmz=0
year=2013
month=3
day=21
timeh=12
jd=JDymd(year,month,day,hour=timeh)
Iglobal=array(0,dim=dim(demm))
deltat=0.5
lat=28.135
lon=-17.247
dayl=daylength(lat,lon,jd,0)
for (srs in seq(dayl[1],dayl[2],deltat)){
	jd=JDymd(year,month,day,hour=srs)
	sv=sunvector(jd,lat,lon,tmz)
	hsh=hillshading(cgr,sv)
	sh=doshade(demm,sv,dl)
	zenith=sunpos(sv)[2]
	Idirdif = insolation(zenith,jd,height,visibility,RH,tempK,0.002,0.15)
	## direct radiation modified by terrain + diffuse irradiation (skyviewfactor ignored)
	## values in J/m^2
	Iglobal = Iglobal + (Idirdif[,1] * hsh + Idirdif[,2] )*3600*deltat
}
## rasterize to plot nicely
Iglobal=raster(Iglobal,crs=projection(dem))
extent(Iglobal)=extent(dem)
plot(Iglobal*1e-6,col=grey(1:100/100),
 	legend.args=list(text=expression(paste('Insolation MJ ',m^-2)), side=4,line=2.5))
contour(dem,lwd=.5,col='sienna1',add=TRUE,levels=seq(0,2500,500))
contour(dem,lwd=.25,col='sienna1',add=TRUE,levels=seq(0,2500,50),drawlabels=FALSE)
unlink(demfile)
}

}

