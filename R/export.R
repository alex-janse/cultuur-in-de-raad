# --- Export en veilige weergave -------------------------------------------------

# Tekst uit een externe bron (Excel-veilig): cellen die beginnen met = + - @
# (of een tab/regeleinde) voert Excel uit als formule ('CSV-injectie'). Een
# apostrof ervoor maakt het gewone tekst.
excel_veilig <- function(x) {
  ifelse(!is.na(x) & grepl("^[=+@\t\r-]", x), paste0("'", x), x)
}

# CSV voor Nederlandse Excel: puntkomma als scheidingsteken, decimale komma
# en een UTF-8-BOM (bytes EF BB BF) zodat letters als 'â' en '€' goed
# overkomen. Tekstkolommen worden Excel-veilig gemaakt.
schrijf_csv <- function(df, bestand) {
  df <- dplyr::mutate(df, dplyr::across(dplyr::where(is.character), excel_veilig))
  writeBin(as.raw(c(0xef, 0xbb, 0xbf)), bestand)
  con <- file(bestand, open = "a", encoding = "UTF-8")
  on.exit(close(con))
  utils::write.csv2(df, con, row.names = FALSE, na = "")
}

# Alleen http(s)-links tonen; voorkomt bv. 'javascript:'-links uit de bron
veilige_link <- function(url) {
  ifelse(grepl("^https?://", url %||% ""), url, "")
}

# Fragment met markeringen (MARK_BEGIN/MARK_EIND) -> veilige HTML: eerst alles
# escapen, dan alleen volledige markeringsparen omzetten in <mark>, en losse
# markeringstekens weghalen. Zo kan er uit de bron geen HTML ontsnappen.
markeer_html <- function(x) {
  x <- htmltools::htmlEscape(x)
  patroon <- sprintf("%s([^%s%s]*)%s", MARK_BEGIN, MARK_BEGIN, MARK_EIND, MARK_EIND)
  x <- gsub(patroon, "<mark>\\1</mark>", x, perl = TRUE)
  gsub(sprintf("[%s%s]", MARK_BEGIN, MARK_EIND), "", x, perl = TRUE)
}
