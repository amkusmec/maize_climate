haversine <- function(long1, long2, lat1, lat2) {
  r <- 6371 # Mean earth radius (km) for the WGS84 ellipsoid
  
  # Convert to radians
  lambda1 <- long1*pi/180
  lambda2 <- long2*pi/180
  phi1 <- lat1*pi/180
  phi2 <- lat2*pi/180
  
  # Absolute differences
  deltaL <- abs(lambda1 - lambda2)
  deltaP <- abs(phi1 - phi2)
  
  # Central angle
  deltaS <- 2*asin(sqrt(sin(deltaP/2)^2 + cos(phi1)*cos(phi2)*sin(deltaL/2)^2))
  
  r*deltaS
}
