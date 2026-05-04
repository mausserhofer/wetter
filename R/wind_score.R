# Positive = tailwind, negative = headwind, 0 = crosswind.
# Average over uniform wind directions = 0.
wind_score <- function(wind_kmh, wind_dir, bearing) {
  tailwind <- -cos((wind_dir - bearing) * pi / 180)
  round(wind_kmh * tailwind, 1)
}
