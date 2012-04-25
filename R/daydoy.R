daydoy <-
function(year,month,day){
	if (nargs() < 3 ) {cat("USAGE: daydoy(year,month,day) \n"); return()}
	as.POSIXlt(ISOdate(year,month,day))$yday+1
}

