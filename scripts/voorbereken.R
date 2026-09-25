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

# Vorig overzicht (van de data-tak) om nieuwe uitkomsten mee te vergelijken
vorig_pad <- file.path(uitvoer, "overzicht.csv")
vorige <- if (file.exists(vorig_pad)) utils::read.csv(vorig_pad) else data.frame()

# Een nieuwe uitkomst die sterk afwijkt van de vorige nacht (met dezelfde
# methode en periode) is verdacht: bv. ontbrekende archieven bij de API.
# Dan liever de vorige versie laten staan dan onzin publiceren.
MAX_AFWIJKING <- 0.2
verdacht <- function(naam, rij) {
  v <- vorige[vorige$set == naam, , drop = FALSE]
  if (nrow(v) != 1 || !all(c("methode", "gemeenten") %in% names(v))) return(NULL)
  if (v$methode != rij$methode || v$periode != rij$periode) return(NULL)
  if (rij$gemeenten < (1 - MAX_AFWIJKING) * v$gemeenten) {
    return(sprintf("%d archieven, vorige keer %d", rij$gemeenten, v$gemeenten))
  }
  if (abs(rij$documenten - v$documenten) > MAX_AFWIJKING * v$documenten) {
    return(sprintf("%d documenten, vorige keer %d", rij$documenten, v$documenten))
  }
  NULL
}

overzicht <- list()
for (naam in names(voorberekende_sets())) {
  termen <- voorberekende_sets()[[naam]]
  message("Zoekvraag ", naam, ": ", paste(termen, collapse = ", "))
  res <- probeer(naam, \() haal_data_op(termen, standaard_periode(), STANDAARD_OPTIES))
  Sys.sleep(PAUZE)
  trends <- if (!is.null(res)) {
    probeer(paste(naam, "trends"),
            \() haal_trends_alle(termen, standaard_periode(), STANDAARD_OPTIES))
  }
  Sys.sleep(PAUZE)

  rij <- if (!is.null(res) && !is.null(trends)) {
    data.frame(
      set = naam, termen = paste(termen, collapse = ", "),
      periode = paste(standaard_periode(), collapse = "-"),
      documenten = res$totaal_docs, gemeenten = nrow(res$per_gemeente),
      methode = methode_versie(),
      sleutel = zoek_sleutel(termen, standaard_periode(), STANDAARD_OPTIES),
      berekend_op = format(res$berekend_op, "%Y-%m-%d %H:%M %Z")
    )
  }
  reden <- if (is.null(rij)) "ophalen mislukt" else verdacht(naam, rij)
  if (!is.null(reden)) {
    message(sprintf("  %s niet bijgewerkt: %s", naam, reden))
    mislukt <- c(mislukt, sprintf("%s (%s)", naam, reden))
    # De vorige versie blijft staan en in het overzicht
    v <- vorige[vorige$set == naam, , drop = FALSE]
    if (nrow(v) == 1) overzicht[[naam]] <- v
    next
  }
  res$trends <- trends
  bewaar(res, sprintf("zoek_%s.rds", rij$sleutel))
  overzicht[[naam]] <- rij
}

if (length(overzicht) > 0) {
  nieuw <- dplyr::bind_rows(overzicht)
  utils::write.csv(nieuw, vorig_pad, row.names = FALSE)
  # Opruimen: zoekresultaten die niet (meer) in het overzicht staan, bv. na
  # een andere methode of periode
  houden <- sprintf("zoek_%s.rds", nieuw$sleutel)
  oud <- setdiff(list.files(uitvoer, pattern = "^zoek_.*[.]rds$"), houden)
  if (length(oud) > 0) {
    file.remove(file.path(uitvoer, oud))
    message("Opgeruimd: ", paste(oud, collapse = ", "))
  }
}

# --- Afsluiting ---------------------------------------------------------------

# mislukt.txt laat de workflow na het publiceren rood worden, zodat een
# gedeeltelijke mislukking opvalt (de geslaagde delen zijn dan wél bijgewerkt)
mislukt_pad <- file.path(uitvoer, "..", "mislukt.txt")
if (length(mislukt) > 0) {
  message("Mislukt of niet bijgewerkt: ", paste(mislukt, collapse = "; "))
  writeLines(mislukt, mislukt_pad)
} else if (file.exists(mislukt_pad)) {
  file.remove(mislukt_pad)
}
aantal_onderdelen <- 1 + length(IV3_KEUZES) + 1 + length(voorberekende_sets())
if (length(mislukt) == aantal_onderdelen) {
  stop("Alles mislukt; niets bijgewerkt.")
}
message("Klaar.")
