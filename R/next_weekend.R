# Returns the upcoming Saturday and Sunday as a Date vector of length 2.
# If today is Saturday, returns today + tomorrow.
# If today is Sunday, returns next Saturday + Sunday.
next_weekend <- function() {
  today <- Sys.Date()
  wday  <- as.integer(format(today, "%u"))   # 1 = Mon … 7 = Sun
  sat   <- today + (6L - wday) %% 7L
  c(sat, sat + 1L)
}
