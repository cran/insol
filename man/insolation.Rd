\name{insolation}
\alias{insolation}

\title{Direct and diffuse solar radiation. }
\description{
Computes direct and diffuse solar irradiance perpendicular to the beam, for a given zenith angle, julian day, altitude and atmospheric conditions.  It does not include topographical factors or sahding. 
}
\usage{
insolation(zenith, jd, height, visibility, RH, tempK, O3, alphag)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{zenith}{ Zenith angle in degrees. }
  \item{jd}{ Julian Day. }
  \item{height}{ Altitude of observer above sea level.}
  \item{visibility}{ Visibility [km]. }
  \item{RH}{ Relative humidity [\%]. }
  \item{tempK}{ Air temperature [K]. }
  \item{O3}{Ozone thickness [m]. }
  \item{alphag}{Albedo of the surrounding terrain [0 to 1] }
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

\seealso{ \code{\link{sunvector}} }

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
cos_inc_sfc=sunv\%*\%as.vector(normalvector(0,0))
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
# Add 10\% diffuse reflcted radiation
lines(meteodate,1.1*Isim,col=3)

}

