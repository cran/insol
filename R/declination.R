declination <-
function (jd) 
{
    if (nargs() < 1) {
        cat("USAGE: declination(jd) \n jd = Julian day \n")
        return()
    }
    # Julian Centuries (Meeus, Astronomical Algorithms 1999. (24.1))
    T = (jd - 2451545)/36525.0
    # mean obliquity of the ecliptic (21.2)
    epsilon = (23+26/60.0+21.448/3600.0) - (46.8150/3600.0)*T - 
    			(0.00059/3600.0)*T^2 + (0.001813/3600.0)*T^3
    # geometric mean longitude of the sun (24.2)
    L0 = 280.46645 + 36000.76983*T + 0.0003032*T^2
    # L0 = (L0 - 360 * (L0%/%360))%%360
    # mean anomaly of the Sun (24.3)
    M = 357.52910 + 35999.05030*T - 0.0001559*T^2 - 0.00000048*T^3
    # eccentricity of the Earth's orbit (24.4)
    e = 0.016708617 - 0.000042037*T - 0.0000001236*T^2
    # Sun's equation of center
    C = (1.914600 - 0.004817*T - 0.000014*T^2)*sin(radians(M)) + 
        (0.019993 - 0.000101*T)*sin(2*radians(M)) +
        0.000290*sin(3*radians(M))
    # Sun's true longitude
    Theta = L0 + C
    # Sun's true anomaly
    v = M + C
    # Sun's Radius Vector (24.5)
    # R = (1.000001018*(1-e^2))/(1 + e*cos(radians(v)))
    #  Longitude of the ascending node of the moon
    Omega = 125.04452 - 1934.136261*T +0.0020708*T^2 +(T^3)/450000
    # Apparent longitude of the sun
    lambda = Theta - 0.00569 - 0.00478*sin(radians(Omega))
    # Sun's declination (24.7)
    delta = asin(sin(radians(epsilon)) * sin(radians(lambda)))
    return(degrees(delta))
}


