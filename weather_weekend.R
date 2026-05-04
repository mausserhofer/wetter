if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(httr2, data.table, gt)

source("R/geocode.R")
source("R/fetch_forecast.R")
source("R/next_weekend.R")
source("R/deg_to_compass.R")
source("R/haversine.R")
source("R/arrival_times.R")

# ── Config ────────────────────────────────────────────────────────────────────

# EuroVelo 6 — Vienna to Budapest (route order)
cities <- data.table(
  name    = c("Vienna", "Bratislava", "Győr", "Komárom",
              "Esztergom", "Visegrád", "Szentendre", "Budapest"),
  country = c("AT", "SK", "HU", "HU", "HU", "HU", "HU", "HU")
)
start_hour <- 6L    # departure from Vienna (local time)
speed_kmh  <- 25    # average cycling speed incl. breaks

# ── Fetch ─────────────────────────────────────────────────────────────────────

message("Geocoding cities...")
locs <- rbindlist(lapply(seq_len(nrow(cities)), function(i)
  geocode(cities$name[i], cities$country[i])))

weekend <- next_weekend()

message("Calculating arrival times...")
arrivals <- rbindlist(lapply(weekend, function(day) {
  start_time <- as.POSIXct(
    sprintf("%s %02d:00:00", format(day, "%Y-%m-%d"), start_hour),
    tz = "Europe/Vienna"
  )
  dt <- arrival_times(locs, start_time, speed_kmh)
  dt[, day := weekdays(day)]
  dt
}))

message("Fetching hourly forecasts...")
forecasts <- rbindlist(lapply(seq_len(nrow(locs)), function(i)
  fetch_forecast(locs$city[i], locs$lat[i], locs$lon[i])
))

# ── Match forecast to arrival hour ────────────────────────────────────────────

arrivals[,  hour_key := format(round(arrival, "hours"), "%Y-%m-%d %H")]
forecasts[, hour_key := format(time, "%Y-%m-%d %H")]

result <- merge(
  arrivals[, .(city, day, dist_cum_km, arrival, hour_key)],
  forecasts[, .(city, hour_key, temp, rain_pct, wind_kmh, wind_dir)],
  by = c("city", "hour_key")
)

# Restore route and day order
result[, city := factor(city, levels = cities$name)]
result[, day  := factor(day,  levels = weekdays(weekend))]
setorder(result, day, city)

display <- result[, .(
  Day         = day,
  City        = city,
  `Arrival`   = format(arrival, "%H:%M"),
  `km`        = round(dist_cum_km),
  `Temp °C`   = temp,
  `Rain %`    = rain_pct,
  `Wind km/h` = wind_kmh,
  `Dir`       = deg_to_compass(wind_dir)
)]

# ── Render table ──────────────────────────────────────────────────────────────

display |>
  gt(groupname_col = "Day") |>
  tab_header(
    title    = "EuroVelo 6 — Cycling Weather Forecast",
    subtitle = sprintf(
      "Vienna → Budapest  |  %s – %s  |  Start %02d:00, %.0f km/h avg",
      format(weekend[1], "%d %b %Y"), format(weekend[2], "%d %b %Y"),
      start_hour, speed_kmh
    )
  ) |>
  cols_align("left",   columns = City) |>
  cols_align("center", columns = c(Arrival, km, `Temp °C`, `Rain %`, `Wind km/h`, Dir)) |>
  cols_label(km = "km from start") |>
  fmt_number(columns  = c(`Temp °C`, `Wind km/h`), decimals = 1) |>
  fmt_integer(columns = c(`Rain %`, km)) |>
  data_color(
    columns = `Temp °C`,
    palette = c("#ffffcc", "#fd8d3c", "#bd0026")
  ) |>
  data_color(
    columns = `Rain %`,
    domain  = c(0L, 100L),
    palette = c("white", "#4a90d9")
  ) |>
  data_color(
    columns = `Wind km/h`,
    palette = c("white", "#a8ddb5", "#0868ac")
  ) |>
  tab_options(
    heading.align             = "left",
    column_labels.font.weight = "bold"
  )
