if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(httr2, data.table, gt)

source("R/geocode.R")
source("R/fetch_forecast.R")
source("R/easter.R")
source("R/austrian_holidays.R")
source("R/next_weekend.R")
source("R/deg_to_compass.R")
source("R/haversine.R")
source("R/arrival_times.R")
source("R/wind_score.R")
source("R/temp_score.R")

# ── Config ────────────────────────────────────────────────────────────────────

# EuroVelo 6 — Vienna to Budapest (route order)
cities <- data.table(
  name    = c("Vienna", "Bratislava", "Győr", "Komárom",
              "Esztergom", "Visegrád", "Szentendre", "Budapest"),
  country = c("AT", "SK", "HU", "HU", "HU", "HU", "HU", "HU")
)
start_hour  <- 6L       # departure used for the detailed forecast table
start_hours <- 5:10    # range explored in the departure-time optimisation
speed_kmh   <- 25      # average cycling speed incl. breaks

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
  dt[, day := format(day, "%a %d %b")]
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

# Route bearing from start to end (used for tailwind calculation)
route_bearing <- bearing_deg(locs$lat[1], locs$lon[1],
                             locs$lat[nrow(locs)], locs$lon[nrow(locs)])

# Restore route and day order
result[, city := factor(city, levels = cities$name)]
result[, day  := factor(day,  levels = format(weekend, "%a %d %b"))]
setorder(result, day, city)

display <- result[, .(
  Day         = day,
  City        = city,
  `Arrival`   = format(arrival, "%H:%M"),
  `km`        = round(dist_cum_km),
  `Temp °C`   = temp,
  `Rain %`    = rain_pct,
  `Wind km/h` = wind_kmh,
  `Dir`       = deg_to_compass(wind_dir),
  `Wind Score` = wind_score(wind_kmh, wind_dir, route_bearing),
  `Temp Score` = temp_score(temp),
  `Score`      = wind_score(wind_kmh, wind_dir, route_bearing) + temp_score(temp)
)]

# ── Render table ──────────────────────────────────────────────────────────────

display |>
  gt(groupname_col = "Day") |>
  tab_header(
    title    = "EuroVelo 6 — Cycling Weather Forecast",
    subtitle = sprintf(
      "Vienna → Budapest  |  %s – %s  |  Start %02d:00, %.0f km/h avg",
      format(min(weekend), "%d %b %Y"), format(max(weekend), "%d %b %Y"),
      start_hour, speed_kmh
    )
  ) |>
  cols_align("left",   columns = City) |>
  cols_align("center", columns = c(Arrival, km, `Temp °C`, `Rain %`, `Wind km/h`, Dir, `Wind Score`, `Temp Score`, Score)) |>
  cols_label(km = "km from start") |>
  fmt_number(columns  = c(`Temp °C`, `Wind km/h`, `Wind Score`, `Temp Score`, Score), decimals = 1) |>
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
  data_color(
    columns = `Wind Score`,
    palette = c("#d73027", "white", "#1a9850")
  ) |>
  data_color(
    columns = `Temp Score`,
    palette = c("#d73027", "white", "#1a9850")
  ) |>
  data_color(
    columns = Score,
    palette = c("#d73027", "white", "#1a9850")
  ) |>
  summary_rows(
    groups  = everything(),
    columns = c(`Wind Score`, `Temp Score`, Score),
    fns     = list(`Trip avg` = ~ round(mean(.), 1)),
    fmt     = list(~ fmt_number(., decimals = 1))
  ) |>
  tab_options(
    heading.align             = "left",
    column_labels.font.weight = "bold"
  )

# ── Departure-time optimisation ───────────────────────────────────────────────

opt <- rbindlist(lapply(weekend, function(day) {
  day_str <- format(day, "%Y-%m-%d")

  rbindlist(lapply(start_hours, function(h) {
    start_time <- as.POSIXct(
      paste0(day_str, sprintf(" %02d:00:00", h)),
      tz = "Europe/Vienna"
    )
    arr       <- arrival_times(locs, start_time, speed_kmh)
    # round to nearest hour without relying on round.POSIXt dispatch:
    # add 30 min (1800 s), then truncate via %H format
    hour_keys <- format(arr$arrival + 1800, "%Y-%m-%d %H")

    matched <- merge(
      data.table(city = arr$city, hour_key = hour_keys),
      forecasts[, .(city, hour_key, wind_kmh, wind_dir, temp)],
      by = c("city", "hour_key")
    )

    data.table(
      day        = format(day, "%a %d %b"),
      start_hour = sprintf("%02d:00", h),
      avg_score  = round(mean(
        wind_score(matched$wind_kmh, matched$wind_dir, route_bearing) +
        temp_score(matched$temp)
      ), 1)
    )
  }))
}))

opt[, day := factor(day, levels = format(weekend, "%a %d %b"))]
setorder(opt, day)

opt_wide <- dcast(opt, day ~ start_hour, value.var = "avg_score")

opt_wide |>
  gt(rowname_col = "day") |>
  tab_header(
    title    = "Best departure time — average trip score",
    subtitle = sprintf("Vienna → Budapest  |  %.0f km/h avg", speed_kmh)
  ) |>
  cols_align("center", columns = everything()) |>
  fmt_number(columns = everything(), decimals = 1) |>
  data_color(
    columns = everything(),
    palette = c("#d73027", "white", "#1a9850")
  ) |>
  tab_options(
    heading.align             = "left",
    column_labels.font.weight = "bold"
  )
