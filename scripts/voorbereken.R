# =============================================================================
# Nachtelijke voorberekening (draait in GitHub Actions, zie
# .github/workflows/voorbereken.yml; lokaal: Rscript scripts/voorbereken.R uitvoer)
#
# Schrijft naar de uitvoermap:
#   zoek_<sleutel>.rds    resultaat per standaardzoekvraag, incl. trends
#   cbs_inwoners.rds, iv3_<keuze>.rds, gemeentegrenzen.rds
#   overzicht.csv         wat er is berekend en wanneer
# Mislukt een onderdeel, dan blijft de vorige versie in de uitvoermap staan.
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
uitvoer <- if (length(args) > 0) args[1] else "uitvoer"
dir.create(uitvoer, showWarnings = FALSE, recursive = TRUE)

# Altijd live ophalen: geen voorberekende data lezen, lege tijdelijke cache.
# timeout: maximum voor downloads via base R (o.a. cbsodataR), zodat een
# hangende verbinding niet de hele nachtelijke taak blokkeert.
options(cultuur.voorberekend_url = NA,
        cultuur.cache_map = file.path(tempdir(), "cache"),
        timeout = 300)
suppressMessages(shiny::loadSupport(".", renv = globalenv()))

PAUZE <- 20  # seconden tussen zware verzoeken, i.v.m. de limiet van de API

# Probeert expr een paar keer; wacht lang bij HTTP 429. Een onvolledig
# antwoord van de API (waarschuwing uit post_json) geldt als fout: dat
# willen we niet bewaren. Andere waarschuwingen laten we gewoon door.
probeer <- function(naam, expr_fn, pogingen = 3) {
  for (i in seq_len(pogingen)) {
    uitkomst <- tryCatch(
      withCallingHandlers(expr_fn(), warning = function(w) {
        # stop(w) zou een waarschuwing blijven; een nieuwe fout wordt wél
        # door tryCatch hieronder opgevangen
        if (grepl("archiefdelen", conditionMessage(w))) stop(conditionMessage(w))
      }),
      error = function(e) e
    )
    if (!inherits(uitkomst, "error")) return(uitkomst)
    melding <- conditionMessage(uitkomst)
    wacht <- if (grepl("429", melding)) 300 else 60
    message(sprintf("  %s: poging %d mislukt (%s)", naam, i, melding))
    if (i < pogingen) Sys.sleep(wacht)
  }
  NULL
}

mislukt <- character()
bewaar <- function(data, bestand) {
  saveRDS(data, file.path(uitvoer, bestand))
  message(sprintf("  opgeslagen: %s", bestand))
}

# --- CBS en gemeentegrenzen ---------------------------------------------------

message("CBS-inwoners")
inw <- probeer("inwoners", haal_inwoners)
if (is.null(inw)) mislukt <- c(mislukt, "inwoners") else bewaar(inw, "cbs_inwoners.rds")

for (keuze in IV3_KEUZES) {
  message("CBS Iv3 ", keuze)
  d <- probeer(keuze, \() haal_cultuurlasten(keuze))
  if (is.null(d)) mislukt <- c(mislukt, keuze) else bewaar(d, sprintf("iv3_%s.rds", keuze))
}

message("Gemeentegrenzen")
g <- probeer("grenzen", haal_gemeentegrenzen)
if (is.null(g)) mislukt <- c(mislukt, "grenzen") else bewaar(g, "gemeentegrenzen.rds")

# --- Standaardzoekvragen ----------------------------------------------------

overzicht <- list()
for (naam in names(voorberekende_sets())) {
  termen <- voorberekende_sets()[[naam]]
  message("Zoekvraag ", naam, ": ", paste(termen, collapse = ", "))
  res <- probeer(naam, \() haal_data_op(termen, VOORBEREKEND_JAREN, STANDAARD_OPTIES))
  Sys.sleep(PAUZE)
  trends <- if (!is.null(res)) {
    probeer(paste(naam, "trends"),
            \() haal_trends_alle(termen, VOORBEREKEND_JAREN, STANDAARD_OPTIES))
  }
  Sys.sleep(PAUZE)
  if (is.null(res) || is.null(trends)) {
    mislukt <- c(mislukt, naam)
    next
  }
  res$trends <- trends
  res$berekend_op <- Sys.time()
  sleutel <- zoek_sleutel(termen, VOORBEREKEND_JAREN, STANDAARD_OPTIES)
  bewaar(res, sprintf("zoek_%s.rds", sleutel))
  overzicht[[naam]] <- data.frame(
    set = naam, termen = paste(termen, collapse = ", "),
    periode = paste(VOORBEREKEND_JAREN, collapse = "-"),
    documenten = res$totaal_docs, sleutel = sleutel,
    berekend_op = format(res$berekend_op, "%Y-%m-%d %H:%M %Z")
  )
}
if (length(overzicht) > 0) {
  utils::write.csv(do.call(rbind, overzicht), file.path(uitvoer, "overzicht.csv"),
                   row.names = FALSE)
}

# --- Afsluiting ---------------------------------------------------------------

if (length(mislukt) > 0) {
  message("Mislukt (vorige versie blijft staan): ", paste(mislukt, collapse = ", "))
}
aantal_onderdelen <- 1 + length(IV3_KEUZES) + 1 + length(voorberekende_sets())
if (length(mislukt) == aantal_onderdelen) {
  stop("Alles mislukt; niets bijgewerkt.")
}
message("Klaar.")
