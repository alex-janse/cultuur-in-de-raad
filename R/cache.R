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
  # Dan de nachtelijk voorberekende versie (alleen als het een tabel is),
  # en pas daarna live ophalen
  voorberekend <- lees_voorberekend(paste0(naam, ".rds"))
  data <- if (is.data.frame(voorberekend)) voorberekend else maak()
  # Op een server met een alleen-lezen map werkt de app gewoon zonder cache
  tryCatch({
    dir.create(cache_map(), showWarnings = FALSE, recursive = TRUE)
    saveRDS(data, pad)
  }, error = function(e) NULL, warning = function(w) NULL)
  data
}

# Geheugencache voor zoekvragen aan OpenBesluitvorming: dezelfde vraag binnen
# een uur (ook van andere gebruikers van dezelfde app) gaat niet opnieuw naar
# de API. Dat scheelt wachttijd en voorkomt HTTP 429 (te veel verzoeken).
ori_cache <- cachem::cache_mem(max_age = 3600, max_size = 200 * 1024^2)

# Eén keer per R-proces laden, gedeeld door alle bezoekers. Een mislukte
# poging wordt een minuut onthouden, zodat niet elke bezoeker (of elke
# herberekening) de trage bron opnieuw probeert.
proces_geheugen <- new.env()

per_proces <- function(naam, maak, fout_geldig = 60) {
  bewaard <- proces_geheugen[[naam]]
  if (!is.null(bewaard)) {
    if (is.null(bewaard$fout)) return(bewaard$data)
    if (Sys.time() < bewaard$geldig_tot) stop(bewaard$fout)
  }
  data <- tryCatch(maak(), error = function(e) {
    proces_geheugen[[naam]] <- list(fout = conditionMessage(e),
                                    geldig_tot = Sys.time() + fout_geldig)
    stop(conditionMessage(e))
  })
  proces_geheugen[[naam]] <- list(data = data, fout = NULL)
  data
}
