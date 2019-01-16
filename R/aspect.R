aspect = function(cgrad,degrees=FALSE){
    if (nargs() < 1) {
        cat("USAGE: aspect(cellgradient, degrees=FALSE) \n cellgradient is a 3D matrix of vnormal vector to surface see ?cgrad")
        return()
    }
    y = cgrad[,,2]
    x = cgrad[,,1]
    y[is.na(y)] = 0
    x[is.na(x)] = 0
	aspect = atan2(y,x) + pi/2
	aspect[aspect<0] = aspect[aspect<0]+2*pi
	if (degrees) aspect = degrees(aspect)
	return(aspect)
}