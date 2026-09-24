# --- Export ------------------------------------------------------------------

# CSV voor Nederlandse Excel: puntkomma als scheidingsteken, decimale komma
# en een UTF-8-BOM (bytes EF BB BF) zodat letters als 'â' en '€' goed
# overkomen.
schrijf_csv <- function(df, bestand) {
  writeBin(as.raw(c(0xef, 0xbb, 0xbf)), bestand)
  con <- file(bestand, open = "a", encoding = "UTF-8")
  on.exit(close(con))
  utils::write.csv2(df, con, row.names = FALSE, na = "")
}

# Alleen http(s)-links tonen; voorkomt bv. 'javascript:'-links uit de bron
veilige_link <- function(url) {
  ifelse(grepl("^https?://", url %||% ""), url, "")
}
