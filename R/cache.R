# --- Caches ------------------------------------------------------------------

# Schijfcache voor data die zelden verandert (CBS, gemeentegrenzen). Overleeft
# een herstart van de app; map staat in .gitignore.
cache_map <- function() getOption("cultuur.cache_map", "cache")

schijf_cache <- function(naam, max_dagen, maak) {
  pad <- file.path(cache_map(), paste0(naam, ".rds"))
  if (file.exists(pad)) {
    leeftijd <- difftime(Sys.time(), file.mtime(pad), units = "days")
    if (leeftijd < max_dagen) return(readRDS(pad))
  }
  data <- maak()
  dir.create(cache_map(), showWarnings = FALSE, recursive = TRUE)
  saveRDS(data, pad)
  data
}

# Geheugencache voor zoekvragen aan OpenBesluitvorming: dezelfde vraag binnen
# een uur (ook van andere gebruikers van dezelfde app) gaat niet opnieuw naar
# de API. Dat scheelt wachttijd en voorkomt HTTP 429 (te veel verzoeken).
ori_cache <- cachem::cache_mem(max_age = 3600, max_size = 200 * 1024^2)
