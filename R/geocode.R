geocode <- function(city, country_code = NULL) {
  n <- if (is.null(country_code)) 1L else 10L

  resp <- request("https://geocoding-api.open-meteo.com/v1/search") |>
    req_url_query(name = city, count = n, language = "en", format = "json") |>
    req_perform()

  res <- resp_body_json(resp)$results
  if (is.null(res) || length(res) == 0L) {
    warning(sprintf("'%s' not found — skipping", city))
    return(NULL)
  }

  if (!is.null(country_code)) {
    codes <- sapply(res, `[[`, "country_code")
    res   <- res[codes == toupper(country_code)]
    if (length(res) == 0L) {
      warning(sprintf("'%s' not found in country '%s' — skipping", city, country_code))
      return(NULL)
    }
  }

  r <- res[[1]]
  data.table(city = city, lat = r$latitude, lon = r$longitude)
}
