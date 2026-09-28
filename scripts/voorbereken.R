# =============================================================================
# Nachtelijke voorberekening (draait in GitHub Actions, zie
# .github/workflows/voorbereken.yml; lokaal: Rscript scripts/voorbereken.R uitvoer)
#
# Schrijft naar de uitvoermap:
#   zoek_<sleutel>.rds    resultaat per standaardzoekvraag, incl. trends
#   cbs_inwoners.rds, iv3_<keuze>.rds, gemeentegrenzen.rds
#   bronversies.rds       welke versie van CBS en PDOK daarin zit
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

# Deze bronnen veranderen een paar keer per jaar. Elke nacht wordt alleen
# gecontroleerd of er een nieuwe versie is (een klein verzoek per tabel); pas
# dan wordt er gedownload. CBS_OPNIEUW=true (handmatige run) haalt alles op.
# Is de bron onbereikbaar bij de controle, dan blijft de vorige versie staan:
# een waarschuwing, geen fout.
forceer <- identical(Sys.getenv("CBS_OPNIEUW"), "true")
versies_pad <- file.path(uitvoer, "bronversies.rds")
versies <- if (file.exists(versies_pad)) readRDS(versies_pad) else leeg_bronversies()
waarschuwingen <- character()
vandaag <- format(Sys.Date())

werk_bij <- function(onderdeel, bestand, versie_fn, haal_fn) {
  message(onderdeel)
  pad <- file.path(uitvoer, bestand)
  rij <- which(versies$onderdeel == onderdeel)
  nieuw <- probeer(paste(onderdeel, "(controle)"), versie_fn, pogingen = 2)
  if (is.null(nieuw)) {
    if (file.exists(pad)) {
      waarschuwingen <<- c(waarschuwingen, onderdeel)
      message("  niet te controleren; de vorige versie blijft staan")
      return(invisible(FALSE))
    }
    nieuw <- list(versie = NA_character_, bron_datum = NA_character_)
  } else if (length(rij) == 1) {
    versies$gecontroleerd[rij] <<- vandaag
    if (!forceer && file.exists(pad) && identical(versies$versie[rij], nieuw$versie)) {
      message(sprintf("  ongewijzigd (%s)", nieuw$versie))
      return(invisible(FALSE))
    }
  }

  data <- probeer(onderdeel, haal_fn)
  if (is.null(data)) {
    mislukt <<- c(mislukt, onderdeel)
    return(invisible(FALSE))
  }
  bewaar(data, bestand)
  regel <- data.frame(onderdeel = onderdeel, versie = nieuw$versie,
                      bron_datum = nieuw$bron_datum, opgehaald = vandaag,
                      gecontroleerd = if (is.na(nieuw$versie)) NA_character_ else vandaag)
  versies <<- rbind(versies[versies$onderdeel != onderdeel, , drop = FALSE], regel)
  invisible(TRUE)
}

werk_bij("inwoners", "cbs_inwoners.rds", \() cbs_versie("inwoners"), haal_inwoners)
# De zoekvragen koppelen archieven aan gemeenten via deze inwoners; zonder
# zouden ze zonder gemeentecodes (kaart, per inwoner) worden gepubliceerd
if (!inwoners_naar_cache(uitvoer)) message("  geen CBS-inwoners beschikbaar")

for (keuze in IV3_KEUZES) {
  werk_bij(keuze, sprintf("iv3_%s.rds", keuze), \() cbs_versie(keuze),
           \() haal_cultuurlasten(keuze))
}

werk_bij("grenzen", "gemeentegrenzen.rds",
         \() list(versie = paste("PDOK", grenzen_jaar()), bron_datum = NA_character_),
         haal_gemeentegrenzen)

# Alleen versies van onderdelen die nog bestaan (bv. niet een oud Iv3-jaar)
versies <- versies[versies$onderdeel %in% c("inwoners", IV3_KEUZES, "grenzen"), ]
saveRDS(versies, versies_pad)


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
  # Het resultaat bevat ook de trends van alle gemeenten
  res <- probeer(naam, \() haal_data_op(termen, standaard_periode(), STANDAARD_OPTIES))
  Sys.sleep(PAUZE)

  rij <- if (!is.null(res)) {
    data.frame(
      set = naam, termen = paste(termen, collapse = ", "),
      periode = paste(standaard_periode(), collapse = "-"),
      documenten = res$totaal_docs, gemeenten = nrow(res$per_gemeente),
      methode = methode_versie(),
      sleutel = zoek_sleutel(termen, standaard_periode(), STANDAARD_OPTIES),
      berekend_op = format(res$berekend_op, "%Y-%m-%d %H:%M %Z")
    )
  }
  reden <- if (is.null(rij)) {
    "ophalen mislukt"
  } else if (!isTRUE(res$inwoners_ok)) {
    # Zonder koppeling aan CBS-gemeenten geen kaart en geen cijfers per inwoner
    "geen CBS-inwoners"
  } else {
    verdacht(naam, rij)
  }
  if (!is.null(reden)) {
    message(sprintf("  %s niet bijgewerkt: %s", naam, reden))
    mislukt <- c(mislukt, sprintf("%s (%s)", naam, reden))
    # De vorige versie blijft staan en in het overzicht
    v <- vorige[vorige$set == naam, , drop = FALSE]
    if (nrow(v) == 1) overzicht[[naam]] <- v
    next
  }
  bewaar(res, sprintf("zoek_%s.rds", rij$sleutel))
  overzicht[[naam]] <- rij

  # Archieven zonder CBS-gemeente: meestal een nieuwe fusie of een afwijkende
  # naam. Die tellen als losse 'gemeente' zonder kaart en inwoners; melden,
  # zodat FUSIES of CBS_NAAM_NAAR_KEY (R/config.R) kan worden aangevuld.
  if (naam == "Standaard") {
    onbekend <- res$per_gemeente$key[is.na(res$per_gemeente$gemeentecode)]
    onbekend_pad <- file.path(uitvoer, "..", "onbekend.txt")
    if (length(onbekend) > 0) {
      message("Archieven zonder CBS-gemeente: ", paste(onbekend, collapse = ", "))
      writeLines(onbekend, onbekend_pad)
    } else if (file.exists(onbekend_pad)) {
      file.remove(onbekend_pad)
    }
  }
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
waarschuwing_pad <- file.path(uitvoer, "..", "niet_gecontroleerd.txt")
if (length(waarschuwingen) > 0) {
  message("Niet te controleren (vorige versie blijft): ",
          paste(waarschuwingen, collapse = "; "))
  writeLines(waarschuwingen, waarschuwing_pad)
} else if (file.exists(waarschuwing_pad)) {
  file.remove(waarschuwing_pad)
}

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
