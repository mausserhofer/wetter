get_forecasts <- function(locs, weekend) {
  fc <- rbindlist(lapply(seq_len(nrow(locs)), function(i)
    fetch_forecast(locs$city[i], locs$lat[i], locs$lon[i], dates = weekend)
  ))
  fc[, hour_key := format(time, "%Y-%m-%d %H")]
  fc
}
