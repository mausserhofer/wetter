if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(httr2, data.table, gt)

invisible(lapply(list.files("R", full.names = TRUE), source))

# ── Config ────────────────────────────────────────────────────────────────────

cities <- data.table(
  name = c(
    "Munich", "Freising", "Moosburg an der Isar", "Landshut",
    "Dingolfing", "Landau an der Isar", "Plattling", "Deggendorf",
    "Vilshofen an der Donau", "Passau",
    "Engelhartszell", "Linz", "Enns", "Grein", "Ybbs an der Donau",
    "Melk", "Spitz an der Donau", "Dürnstein", "Krems an der Donau",
    "Tulln an der Donau", "Klosterneuburg", "Vienna",
    "Schwechat", "Bad Deutsch-Altenburg", "Hainburg an der Donau",
    "Bratislava", "Rajka", "Mosonmagyaróvár", "Győr", "Komárom", "Tata",
    "Esztergom", "Zebegény", "Nagymaros", "Visegrád",
    "Vác", "Szentendre", "Budapest"
  ),
  country = c(
    "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE",
    "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT",
    "AT", "AT", "AT", "SK", "HU", "HU", "HU", "HU", "HU",
    "HU", "HU", "HU", "HU", "HU", "HU", "HU"
  )
)
milestones <- c("Munich", "Linz", "Vienna", "Budapest")
start_hour <- 6L
speed_kmh  <- 25

# ── Run ───────────────────────────────────────────────────────────────────────

locs      <- get_coordinates(cities)
weekend   <- next_weekend()
forecasts <- get_forecasts(locs, weekend)

# All forward route pairs between milestones
pairs <- CJ(from = milestones, to = milestones)[from != to & (from == "Vienna" | to == "Vienna")]
pairs[, other := ifelse(from == "Vienna", to, from)]
pairs[, dir   := ifelse(from == "Vienna", 1L, 2L)]
setorder(pairs, match(other, milestones), dir)
pairs[, c("other", "dir") := NULL]

scores <- rbindlist(lapply(seq_len(nrow(pairs)), function(i) {
  route_locs <- locs[seq(which(locs$city == pairs$from[i]),
                         which(locs$city == pairs$to[i]))]
  rbindlist(lapply(weekend, function(day) {
    arr       <- arrival_times(route_locs,
                   as.POSIXct(paste0(format(day, "%Y-%m-%d"),
                                     sprintf(" %02d:00:00", start_hour)),
                              tz = "Europe/Vienna"),
                   speed_kmh)
    hour_keys <- format(arr$arrival + 1800L, "%Y-%m-%d %H")
    matched   <- merge(
      merge(data.table(city = arr$city, hour_key = hour_keys),
            forecasts[, .(city, hour_key, wind_kmh, wind_dir, temp)],
            by = c("city", "hour_key")),
      route_locs[, .(city, segment_bearing)],
      by = "city"
    )
    data.table(
      route     = sprintf("%s → %s", pairs$from[i], pairs$to[i]),
      day       = format(day, "%a %d %b"),
      avg_score = round(mean(
        wind_score(matched$wind_kmh, matched$wind_dir, matched$segment_bearing) +
        temp_score(matched$temp)
      ), 1)
    )
  }))
}))

# ── Render ────────────────────────────────────────────────────────────────────

scores[, day   := factor(day,   levels = format(weekend, "%a %d %b"))]
scores[, route := factor(route, levels = unique(route))]
setorder(scores, route, day)

dcast(scores, route ~ day, value.var = "avg_score") |>
  gt(rowname_col = "route") |>
  tab_header(
    title    = "Route comparison — average trip score",
    subtitle = sprintf("Start %02d:00, %.0f km/h avg", start_hour, speed_kmh)
  ) |>
  cols_align("center", columns = everything()) |>
  fmt_number(columns = everything(), decimals = 1) |>
  data_color(columns = everything(), palette = c("#d73027", "white", "#1a9850")) |>
  tab_options(heading.align = "left", column_labels.font.weight = "bold")
