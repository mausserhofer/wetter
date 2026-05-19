render_opt_table <- function(locs, forecasts, weekend, start_hours, speed_kmh) {
  opt <- rbindlist(lapply(weekend, function(day) {
    day_str <- format(day, "%Y-%m-%d")
    rbindlist(lapply(start_hours, function(h) {
      arr       <- arrival_times(locs,
                    as.POSIXct(paste0(day_str, sprintf(" %02d:00:00", h)),
                               tz = "Europe/Vienna"),
                    speed_kmh)
      hour_keys <- format(arr$arrival + 1800L, "%Y-%m-%d %H")
      matched   <- merge(
        merge(data.table(city = arr$city, hour_key = hour_keys),
              forecasts[, .(city, hour_key, wind_kmh, wind_dir, temp, rain_mm)],
              by = c("city", "hour_key")),
        locs[, .(city, segment_bearing)],
        by = "city"
      )
      data.table(
        day        = format(day, "%a %d %b"),
        start_hour = sprintf("%02d:00", h),
        avg_score  = round(mean(
          wind_score(matched$wind_kmh, matched$wind_dir, matched$segment_bearing) +
          temp_score(matched$temp) +
          rain_score(matched$rain_mm)
        ), 1)
      )
    }))
  }))

  opt[, day := factor(day, levels = format(weekend, "%a %d %b"))]
  setorder(opt, day)

  route <- sprintf("%s → %s", locs$city[1L], locs$city[nrow(locs)])
  dcast(opt, day ~ start_hour, value.var = "avg_score") |>
    gt(rowname_col = "day") |>
    tab_header(title    = "Best departure time — average trip score",
               subtitle = sprintf("%s  |  %.0f km/h avg", route, speed_kmh)) |>
    cols_align("center", columns = everything()) |>
    fmt_number(columns = everything(), decimals = 1) |>
    data_color(columns = everything(), domain = c(-10, 10), palette = c("#d73027", "white", "#1a9850")) |>
    tab_options(heading.align = "left", column_labels.font.weight = "bold")
}
