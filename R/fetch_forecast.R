fetch_forecast <- function(city, lat, lon) {
  resp <- request("https://api.open-meteo.com/v1/forecast") |>
    req_url_query(
      latitude      = lat,
      longitude     = lon,
      hourly        = paste(
        "temperature_2m", "precipitation_probability",
        "windspeed_10m", "winddirection_10m",
        sep = ","
      ),
      timezone      = "Europe/Vienna",
      forecast_days = 7L
    ) |>
    req_perform()

  h <- resp_body_json(resp)$hourly
  data.table(
    city     = city,
    time     = as.POSIXct(unlist(h$time), format = "%Y-%m-%dT%H:%M",
                          tz = "Europe/Vienna"),
    temp     = as.numeric(unlist(h$temperature_2m)),
    rain_pct = as.integer(unlist(h$precipitation_probability)),
    wind_kmh = as.numeric(unlist(h$windspeed_10m)),
    wind_dir = as.numeric(unlist(h$winddirection_10m))
  )
}
