# locs      : data.table with columns city, lat, lon (in route order)
# start     : POSIXct departure time at first city
# speed_kmh : average speed in km/h (incl. breaks)
arrival_times <- function(locs, start, speed_kmh) {
  n       <- nrow(locs)
  dist_km <- numeric(n)
  for (i in seq(2, n)) {
    dist_km[i] <- haversine_km(
      locs$lat[i - 1], locs$lon[i - 1],
      locs$lat[i],     locs$lon[i]
    )
  }
  hours_elapsed <- cumsum(dist_km) / speed_kmh
  data.table(
    city         = locs$city,
    dist_cum_km  = cumsum(dist_km),
    arrival      = start + hours_elapsed * 3600
  )
}
