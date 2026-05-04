# Meeus/Jones/Butcher algorithm
easter_date <- function(year) {
  a <- year %% 19
  b <- year %/% 100
  c <- year %% 100
  d <- b %/% 4
  e <- b %% 4
  f <- (b + 8L) %/% 25
  g <- (b - f + 1L) %/% 3
  h <- (19L * a + b - d - g + 15L) %% 30
  i <- c %/% 4
  k <- c %% 4
  l <- (32L + 2L * e + 2L * i - h - k) %% 7
  m <- (a + 11L * h + 22L * l) %/% 451
  month <- (h + l - 7L * m + 114L) %/% 31
  day   <- (h + l - 7L * m + 114L) %% 31 + 1L
  as.Date(sprintf("%d-%02d-%02d", year, month, day))
}
