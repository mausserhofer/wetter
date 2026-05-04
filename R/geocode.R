geocode <- function(city) {
  resp <- request("https://geocoding-api.open-meteo.com/v1/search") |>
    req_url_query(name = city, count = 1L, language = "en", format = "json") |>
    req_perform()

  res <- resp_body_json(resp)$results
  if (is.null(res) || length(res) == 0L) {
    warning(sprintf("'%s' not found — skipping", city))
    return(NULL)
  }
  r <- res[[1]]
  data.table(city = city, lat = r$latitude, lon = r$longitude)
}
