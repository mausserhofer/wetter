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
  dt <- data.table(
    city     = city,
    time     = as.POSIXct(unlist(h$time), format = "%Y-%m-%dT%H:%M",
                          tz = "Europe/Vienna"),
    temp     = as.numeric(unlist(h$temperature_2m)),
    rain_pct = as.integer(unlist(h$precipitation_probability)),
    rain_mm  = as.numeric(unlist(h$precipitation)),
    wind_kmh = as.numeric(unlist(h$windspeed_10m)),
    wind_dir = as.numeric(unlist(h$winddirection_10m))
  )
  dt[as.Date(time, tz = "Europe/Vienna") %in% dates]
}
