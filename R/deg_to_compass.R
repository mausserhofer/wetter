deg_to_compass <- function(deg) {
  labels <- c("N", "NO", "O", "SO", "S", "SW", "W", "NW")
  labels[(floor((deg + 22.5) / 45) %% 8) + 1L]
}
