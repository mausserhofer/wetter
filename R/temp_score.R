# Asymmetric parabola: peaks at +0.3 at 18 °C, zero at 10 °C and 28 °C,
# negative outside that range.
temp_score <- function(temp) {
  sigma <- ifelse(temp < 18, 8, 10)
  round(0.3 * (1 - ((temp - 18) / sigma)^2), 1)
}
