# Quadratic penalty above 0.3 mm/h, capped at -20.
# 0.0 mm/h →  0   (dry)
# 0.3 mm/h →  0   (drizzle, acceptable)
# 1.0 mm/h → -3.9 (light rain)
# 2.0 mm/h → -20  (moderate rain, cap)
rain_score <- function(rain_mm) {
  round(pmax(-pmax(rain_mm - 0.3, 0)^2 * 8, -20), 1)
}
