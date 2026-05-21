pacman::p_load(httr2, data.table, gt, webshot2, base64enc, scales)

invisible(lapply(list.files("R", full.names = TRUE), source))

# ── Config ────────────────────────────────────────────────────────────────────
cities <- data.table(
  name = c(
    # Munich → Passau (Isar river route)
    "Munich", "Freising", "Moosburg an der Isar", "Landshut",
    "Dingolfing", "Landau an der Isar", "Plattling", "Deggendorf",
    "Vilshofen an der Donau", "Passau",
    # Passau → Vienna (Danube / EuroVelo 6)
    "Engelhartszell", "Linz", "Enns", "Grein", "Ybbs an der Donau",
    "Melk", "Spitz an der Donau", "Dürnstein", "Krems an der Donau",
    "Tulln an der Donau", "Klosterneuburg", "Vienna",
    # Vienna → Budapest (EuroVelo 6)
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
start_city  <- "Vienna"
end_city    <- "Budapest"
start_hour  <- 6L    # departure used for the detailed forecast table
start_hours <- 5:10  # range explored in the departure-time optimisation
speed_kmh   <- 25    # average cycling speed incl. breaks

# ── Run ───────────────────────────────────────────────────────────────────────
route     <- cities[seq(which(cities$name == start_city),
                        which(cities$name == end_city))]
locs      <- get_coordinates(route)
weekend   <- c(next_weekend())
forecasts <- get_forecasts(locs, weekend)

render_opt_table(locs, forecasts, weekend, start_hours, speed_kmh) |>
  gtsave("options.png")

locs_back <- locs[.N:1]
locs_back[, segment_bearing := c(
  bearing_deg(lat[-.N], lon[-.N], lat[-1], lon[-1]),
  bearing_deg(lat[.N - 1L], lon[.N - 1L], lat[.N], lon[.N])
)]

render_opt_table(locs_back, forecasts, weekend, start_hours, speed_kmh) |>
  gtsave("options_back.png")

result    <- build_result(locs, forecasts, weekend, start_hour, speed_kmh)
render_forecast_table(result, weekend, start_hour, speed_kmh) |>
  gtsave("forecasts.png")

b64 <- function(f) base64encode(f)
template <- paste(readLines("email_template.html", warn = FALSE), collapse = "\n")
body <- gsub("\\{opt_fwd\\}",       b64("options.png"),      template)
body <- gsub("\\{opt_back\\}",      b64("options_back.png"), body)
body <- gsub("\\{forecasts\\}",     b64("forecasts.png"),    body)
body <- gsub("\\{date_label\\}",    format(weekend[1], "%B %d, %Y"), body)
body <- gsub("\\{date_generated\\}", format(Sys.time(), "%Y-%m-%d %H:%M UTC"), body)
writeLines(body, "email_body.html")

