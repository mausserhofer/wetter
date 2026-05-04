if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(httr2, data.table, gt)

source("R/geocode.R")
source("R/fetch_forecast.R")
source("R/next_weekend.R")

# ── Config ────────────────────────────────────────────────────────────────────

# EuroVelo 6 — Vienna to Budapest (route order)
cities <- c(
  "Vienna",
  "Bratislava",
  "Győr",
  "Komárom",
  "Esztergom",
  "Visegrád",
  "Szentendre",
  "Budapest"
)

# ── Fetch ─────────────────────────────────────────────────────────────────────

message("Geocoding cities...")
locs <- rbindlist(lapply(cities, geocode))

message("Fetching forecasts...")
forecasts <- rbindlist(lapply(seq_len(nrow(locs)), function(i)
  fetch_forecast(locs$city[i], locs$lat[i], locs$lon[i])
))

# ── Filter & reshape ──────────────────────────────────────────────────────────

weekend <- next_weekend()
result  <- forecasts[date %in% weekend]

# Preserve route order
result[, city := factor(city, levels = cities)]
result[, day  := weekdays(date)]
setorder(result, date, city)

display <- result[, .(
  City        = city,
  Day         = day,
  Date        = date,
  `Max °C`    = temp_max,
  `Min °C`    = temp_min,
  `Rain %`    = rain_pct,
  `Wind km/h` = wind_kmh
)]

# ── Render table ──────────────────────────────────────────────────────────────

display |>
  gt(groupname_col = "Day") |>
  tab_header(
    title    = "EuroVelo 6 — Weekend Weather Forecast",
    subtitle = sprintf(
      "Vienna → Budapest  |  %s – %s",
      format(weekend[1], "%d %b %Y"),
      format(weekend[2], "%d %b %Y")
    )
  ) |>
  cols_hide(Date) |>
  cols_align("left",   columns = City) |>
  cols_align("center", columns = c(`Max °C`, `Min °C`, `Rain %`, `Wind km/h`)) |>
  fmt_number(columns  = c(`Max °C`, `Min °C`, `Wind km/h`), decimals = 1) |>
  fmt_integer(columns = `Rain %`) |>
  data_color(
    columns = `Max °C`,
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
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) |>
  tab_options(
    heading.align             = "left",
    column_labels.font.weight = "bold"
  )
