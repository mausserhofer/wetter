# Returns all dates in the next 14 days that are Saturday, Sunday,
# or an Austrian public holiday, sorted ascending.
next_weekend <- function(lookahead = 14L) {
  today    <- Sys.Date()
  window   <- seq(today + 1L, today + lookahead, by = "day")
  years    <- unique(as.integer(format(window, "%Y")))
  holidays <- do.call(c, lapply(years, austrian_holidays))
  wday     <- as.integer(format(window, "%u"))  # 1 = Mon … 7 = Sun
  sort(unique(window[wday >= 6L | window %in% holidays]))
}
