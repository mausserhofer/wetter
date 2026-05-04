build_result <- function(locs, forecasts, weekend, start_hour, speed_kmh) {
  arrivals <- rbindlist(lapply(weekend, function(day) {
    dt <- arrival_times(locs,
      as.POSIXct(sprintf("%s %02d:00:00", format(day, "%Y-%m-%d"), start_hour),
                 tz = "Europe/Vienna"),
      speed_kmh)
    dt[, day := format(day, "%a %d %b")]
    dt
  }))
  arrivals[, hour_key := format(arrival + 1800L, "%Y-%m-%d %H")]

  result <- merge(
    merge(
      arrivals[, .(city, day, dist_cum_km, arrival, hour_key)],
      forecasts[, .(city, hour_key, temp, rain_pct, wind_kmh, wind_dir)],
      by = c("city", "hour_key")
    ),
    locs[, .(city, segment_bearing)],
    by = "city"
  )

  result[, city := factor(city, levels = locs$city)]
  result[, day  := factor(day,  levels = format(weekend, "%a %d %b"))]
  setorder(result, day, city)

  result[, .(
    Day          = day,
    City         = city,
    Arrival      = format(arrival, "%H:%M"),
    km           = round(dist_cum_km),
    `Temp °C`    = temp,
    `Rain %`     = rain_pct,
    `Wind km/h`  = wind_kmh,
    Dir          = deg_to_compass(wind_dir),
    `Wind Score` = wind_score(wind_kmh, wind_dir, segment_bearing),
    `Temp Score` = temp_score(temp),
    Score        = wind_score(wind_kmh, wind_dir, segment_bearing) + temp_score(temp)
  )]
}
