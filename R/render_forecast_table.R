render_forecast_table <- function(result, weekend, start_hour, speed_kmh) {
  lvls  <- levels(result$City)
  route <- sprintf("%s → %s", lvls[1L], lvls[length(lvls)])
  result |>
    gt(groupname_col = "Day") |>
    tab_header(
      title    = sprintf("EuroVelo 6 — %s", route),
      subtitle = sprintf("%s  |  %s – %s  |  Start %02d:00, %.0f km/h avg",
                         route,
                         format(min(weekend), "%d %b %Y"),
                         format(max(weekend), "%d %b %Y"),
                         start_hour, speed_kmh)
    ) |>
    cols_align("left",   columns = City) |>
    cols_align("center", columns = c(Arrival, km, `Temp °C`, `Rain %`,
                                     `Wind km/h`, Dir, `Wind Score`, `Temp Score`, Score)) |>
    cols_label(km = "km from start") |>
    fmt_number(columns  = c(`Temp °C`, `Wind km/h`, `Wind Score`, `Temp Score`, Score),
               decimals = 1) |>
    fmt_integer(columns = c(`Rain %`, km)) |>
    data_color(columns = `Temp °C`,   palette = c("#ffffcc", "#fd8d3c", "#bd0026")) |>
    data_color(columns = `Rain %`,    domain  = c(0L, 100L),
                                      palette = c("white", "#4a90d9")) |>
    data_color(columns = `Wind km/h`, palette = c("white", "#a8ddb5", "#0868ac")) |>
    data_color(columns = `Wind Score`, palette = c("#d73027", "white", "#1a9850")) |>
    data_color(columns = `Temp Score`, palette = c("#d73027", "white", "#1a9850")) |>
    data_color(columns = Score,        palette = c("#d73027", "white", "#1a9850")) |>
    summary_rows(
      groups  = everything(),
      columns = c(`Wind Score`, `Temp Score`, Score),
      fns     = list(`Trip avg` = ~ round(mean(.), 1)),
      fmt     = list(~ fmt_number(., decimals = 1))
    ) |>
    tab_options(heading.align = "left", column_labels.font.weight = "bold")
}
