austrian_holidays <- function(year) {
  e <- easter_date(year)
  as.Date(c(
    sprintf("%d-01-01", year),  # Neujahr
    sprintf("%d-01-06", year),  # Heilige Drei Könige
    as.character(e + 1L),        # Ostermontag
    sprintf("%d-05-01", year),  # Staatsfeiertag
    as.character(e + 39L),      # Christi Himmelfahrt
    as.character(e + 50L),      # Pfingstmontag
    as.character(e + 60L),      # Fronleichnam
    sprintf("%d-08-15", year),  # Mariä Himmelfahrt
    sprintf("%d-10-26", year),  # Nationalfeiertag
    sprintf("%d-11-01", year),  # Allerheiligen
    sprintf("%d-12-08", year),  # Mariä Empfängnis
    sprintf("%d-12-25", year),  # Christtag
    sprintf("%d-12-26", year)   # Stefanitag
  ))
}
