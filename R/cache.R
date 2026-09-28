# --- Caches ------------------------------------------------------------------

# Schijfcache voor data die zelden verandert (CBS, gemeentegrenzen). Overleeft
# een herstart van de app; map staat in .gitignore.
cache_map <- function() getOption("cultuur.cache_map", "cache")

schijf_cache <- function(naam, max_dagen, maak) {
  # Eerst de nachtelijk voorberekende versie (alleen als het een tabel is):
  # die is het actueelst, en lees_voorberekend onthoudt hem een uur
  voorberekend <- lees_voorberekend(paste0(naam, ".rds"))
  if (is.data.frame(voorberekend)) return(voorberekend)

  # Dan de schijfcache. Een kapot bestand (bv. half geschreven) negeren
  pad <- file.path(cache_map(), paste0(naam, ".rds"))
  if (file.exists(pad)) {
    leeftijd <- difftime(Sys.time(), file.mtime(pad), units = "days")
    if (leeftijd < max_dagen) {
      data <- tryCatch(readRDS(pad), error = function(e) NULL)
      if (!is.null(data)) return(data)
    }
  }

  # En pas daarna live ophalen
  data <- maak()
  # Via een tijdelijk bestand, zodat een onderbroken schrijfactie geen half
  # bestand achterlaat. Op een server met een alleen-lezen map werkt de app
  # gewoon zonder cache.
  tryCatch({
    dir.create(cache_map(), showWarnings = FALSE, recursive = TRUE)
    tmp <- tempfile(tmpdir = cache_map(), fileext = ".tmp")
    saveRDS(data, tmp)
    if (!file.rename(tmp, pad)) unlink(tmp)
  }, error = function(e) NULL, warning = function(w) NULL)
  data
}

# Geheugencache voor zoekvragen aan OpenBesluitvorming: dezelfde vraag binnen
# een uur (ook van andere gebruikers van dezelfde app) gaat niet opnieuw naar
# de API. Dat scheelt wachttijd en voorkomt HTTP 429 (te veel verzoeken).
ori_cache <- cachem::cache_mem(max_age = 3600, max_size = 200 * 1024^2)

# Eén keer per R-proces laden, gedeeld door alle bezoekers, en na een dag
# opnieuw (dan komen nieuwe nachtelijke cijfers in beeld). Een mislukte
# poging wordt fout_geldig seconden onthouden, zodat niet elke bezoeker (of
# elke herberekening) de trage bron opnieuw probeert.
proces_geheugen <- new.env()

per_proces <- function(naam, maak, fout_geldig = 60, geldig = 24 * 3600) {
  bewaard <- proces_geheugen[[naam]]
  if (!is.null(bewaard) && Sys.time() < bewaard$geldig_tot) {
    if (is.null(bewaard$fout)) return(bewaard$data)
    stop(bewaard$fout)
  }
  data <- tryCatch(maak(), error = function(e) {
    proces_geheugen[[naam]] <- list(fout = conditionMessage(e),
                                    geldig_tot = Sys.time() + fout_geldig)
    stop(conditionMessage(e))
  })
  proces_geheugen[[naam]] <- list(data = data, fout = NULL,
                                  geldig_tot = Sys.time() + geldig)
  data
}
