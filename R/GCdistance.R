GCdistance <-
function(lat1,lon1,lat2,lon2){
# see http://mathworld.wolfram.com/GreatCircle.html  https://edwilliams.org/avform.htm#Dist
	if (nargs() < 2 ) {cat("USAGE: result = distance(lat1,lon1,lat2,lon2)\n coord=c(lat,lon) in decimal degrees \n"); return()}
	lat1 = lat1*pi/180
	lon1 = lon1*pi/180
	lat2 = lat2*pi/180
	lon2 = lon2*pi/180
	EarthR = 6.3756766E6 
	distance = EarthR*2*asin(sqrt((sin((lat1-lat2)/2))^2 + 
                 cos(lat1)*cos(lat2)*(sin((lon1-lon2)/2))^2))
	return(distance)
}

