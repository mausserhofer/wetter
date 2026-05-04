fetch_forecast <- function(city, lat, lon) {
  resp <- request("https://api.open-meteo.com/v1/forecast") |>
    req_url_query(
      latitude      = lat,
      longitude     = lon,
      daily         = paste(
        "temperature_2m_max", "temperature_2m_min",
        "precipitation_probability_max", "windspeed_10m_max",
        sep = ","
      ),
      timezone      = "auto",
      forecast_days = 7L
    ) |>
    req_perform()

  d <- resp_body_json(resp)$daily
  data.table(
    city     = city,
    date     = as.Date(unlist(d$time)),
    temp_max = as.numeric(unlist(d$temperature_2m_max)),
    temp_min = as.numeric(unlist(d$temperature_2m_min)),
    rain_pct = as.integer(unlist(d$precipitation_probability_max)),
    wind_kmh = as.numeric(unlist(d$windspeed_10m_max))
  )
}
