get_coordinates <- function(cities) {
  locs <- rbindlist(lapply(seq_len(nrow(cities)), function(i)
    geocode(cities$name[i], cities$country[i])
  ))
  n <- nrow(locs)
  locs[, segment_bearing := c(
    bearing_deg(lat[-n], lon[-n], lat[-1], lon[-1]),
    bearing_deg(lat[n - 1L], lon[n - 1L], lat[n], lon[n])
  )]
  locs
}
