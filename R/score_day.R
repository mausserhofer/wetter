# Cycling suitability score for a single city/time observation.
#
# Wind component  : wind_kmh * tailwind_factor
#   tailwind_factor = 1  (perfect tailwind)
#                  = 0  (crosswind)
#                  = -1 (perfect headwind)
#   Average over uniform wind directions = 0.
#
# Temperature component : asymmetric parabola
#   peaks at +1 at 18 °C
#   zero at 10 °C (left) and 28 °C (right)
#   negative outside that range
#   weighted at 0.3 so wind dominates
#
# Rain component : quadratic penalty above 20 % probability
#   ≤20 % → 0, 50 % → -4, 80 % → -16, 100 % → -28
score_day <- function(wind_kmh, wind_dir, temp, rain_mm, bearing) {
  tailwind   <- -cos((wind_dir - bearing) * pi / 180)
  wind_score <- wind_kmh * tailwind

  sigma      <- ifelse(temp < 18, 8, 10)   # asymmetric: tighter on cold side
  temp_score <- 1 - ((temp - 18) / sigma)^2

  round(wind_score + 0.3 * temp_score + rain_score(rain_mm), 1)
}
