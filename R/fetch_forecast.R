fetch_forecast <- function(city, lat, lon, dates) {
  resp <- request("https://api.open-meteo.com/v1/forecast") |>
    req_url_query(
      latitude   = lat,
      longitude  = lon,
      hourly     = paste(
        "temperature_2m", "precipitation_probability", "precipitation",
        "windspeed_10m", "winddirection_10m",
        sep = ","
      ),
      timezone   = "Europe/Vienna",
      start_date = format(min(dates), "%Y-%m-%d"),
      end_date   = format(max(dates) + 1L, "%Y-%m-%d")
    ) |>
    req_perform()

  h <- resp_body_json(resp)$hourly
  times <- as.POSIXct(unlist(h$time), format = "%Y-%m-%dT%H:%M",
                      tz = "Europe/Vienna")
  n <- length(times)
  pad <- function(x, FUN = as.numeric) { v <- FUN(unlist(x)); length(v) <- n; v }
  dt <- data.table(
    city     = city,
    time     = times,
    temp     = pad(h$temperature_2m),
    rain_pct = pad(h$precipitation_probability, as.integer),
    rain_mm  = pad(h$precipitation),
    wind_kmh = pad(h$windspeed_10m),
    wind_dir = pad(h$winddirection_10m)
  )
  dt[as.Date(time, tz = "Europe/Vienna") %in% dates]
}
