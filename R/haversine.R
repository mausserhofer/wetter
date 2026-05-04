haversine_km <- function(lat1, lon1, lat2, lon2) {
  r    <- 6371
  phi1 <- lat1 * pi / 180
  phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlam <- (lon2 - lon1) * pi / 180
  a    <- sin(dphi / 2)^2 + cos(phi1) * cos(phi2) * sin(dlam / 2)^2
  2 * r * asin(sqrt(a))
}

# Forward azimuth in degrees (0 = N, 90 = E, 180 = S, 270 = W)
bearing_deg <- function(lat1, lon1, lat2, lon2) {
  phi1 <- lat1 * pi / 180
  phi2 <- lat2 * pi / 180
  dlam <- (lon2 - lon1) * pi / 180
  x    <- sin(dlam) * cos(phi2)
  y    <- cos(phi1) * sin(phi2) - sin(phi1) * cos(phi2) * cos(dlam)
  (atan2(x, y) * 180 / pi + 360) %% 360
}
