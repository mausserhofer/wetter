get_user_input <- function(cities) {
  start_city <- dlgList(cities$name,
                        title = "Startort wählen")$res
  if (!length(start_city)) stop("Kein Startort gewählt.")

  after_start <- cities$name[seq(which(cities$name == start_city) + 1L,
                                 nrow(cities))]
  end_city <- dlgList(after_start,
                      title = "Zielort wählen")$res
  if (!length(end_city)) stop("Kein Zielort gewählt.")

  hour_str   <- dlgInput("Abfahrtszeit für Detailvorhersage (Stunde):", "6")$res
  start_hour <- as.integer(trimws(hour_str))

  range_str   <- dlgInput("Abfahrtszeitraum für Optimierung (z.B. 5:10):", "5:10")$res
  parts       <- as.integer(strsplit(trimws(range_str), "[:\\-]")[[1L]])
  start_hours <- seq(parts[1L], parts[2L])

  list(start_city  = start_city,
       end_city    = end_city,
       start_hour  = start_hour,
       start_hours = start_hours)
}
