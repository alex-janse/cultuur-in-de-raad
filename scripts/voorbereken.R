# =============================================================================
# Nachtelijke voorberekening (draait in GitHub Actions, zie
# .github/workflows/voorbereken.yml; lokaal: Rscript scripts/voorbereken.R uitvoer)
#
# Schrijft naar de uitvoermap:
#   zoek_<sleutel>.rds    resultaat per standaardzoekvraag, incl. trends
#   cbs_inwoners.rds, iv3_<keuze>.rds, gemeentegrenzen.rds
#   bronversies.rds       welke versie van CBS en PDOK daarin zit
#   overzicht.csv         wat er is berekend en wanneer
#   afgekeurd.csv         verdachte uitkomsten van deze nacht (ter bevestiging)
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
begin <- Sys.time()

# De beslissingen (bijwerken, overslaan, afkeuren) staan in R/nachtrun.R
staat <- nieuwe_run(uitvoer, forceer = identical(Sys.getenv("CBS_OPNIEUW"), "true"))

# --- CBS en gemeentegrenzen ---------------------------------------------------

# Deze bronnen veranderen een paar keer per jaar. Elke nacht wordt alleen
# gecontroleerd of er een nieuwe versie is (een klein verzoek per tabel); pas
# dan wordt er gedownload. CBS_OPNIEUW=true (handmatige run) haalt alles op.
# Is de bron onbereikbaar bij de controle, dan blijft de vorige versie staan:
# een waarschuwing, geen fout.
werk_bij(staat, "inwoners", "cbs_inwoners.rds", \() cbs_versie("inwoners"),
         \(nieuw) haal_inwoners())
# De zoekvragen koppelen archieven aan gemeenten via deze inwoners; zonder
# zouden ze zonder gemeentecodes (kaart, per inwoner) worden gepubliceerd
if (!inwoners_naar_cache(uitvoer)) message("  geen CBS-inwoners beschikbaar")

for (keuze in IV3_KEUZES) {
  werk_bij(staat, keuze, sprintf("iv3_%s.rds", keuze), \() cbs_versie(keuze),
           \(nieuw) haal_cultuurlasten(keuze))
}

# Precies het jaar downloaden dat bij de controle is gevonden
werk_bij(staat, "grenzen", "gemeentegrenzen.rds",
         \() {
           jaar <- grenzen_jaar()
           list(versie = paste("PDOK", jaar), bron_datum = NA_character_, jaar = jaar)
         },
         \(nieuw) if (is.null(nieuw$jaar)) haal_gemeentegrenzen() else
           haal_gemeentegrenzen(nieuw$jaar))

# Alleen versies van onderdelen die nog bestaan (bv. niet een oud Iv3-jaar)
versies <- staat$versies
versies <- versies[versies$onderdeel %in% c("inwoners", IV3_KEUZES, "grenzen"), ]
saveRDS(versies, file.path(uitvoer, "bronversies.rds"))


# --- Standaardzoekvragen ----------------------------------------------------

# Vorig overzicht (van de data-tak) om nieuwe uitkomsten mee te vergelijken,
# en de uitkomsten die de vorige nacht als verdacht zijn afgekeurd
vorig_pad <- file.path(uitvoer, "overzicht.csv")
vorige <- lees_overzicht(vorig_pad)
afgekeurd_pad <- file.path(uitvoer, "afgekeurd.csv")
afgekeurd <- lees_overzicht(afgekeurd_pad)
nieuw_afgekeurd <- list()

regel_van <- function(d, naam) {
  if (nrow(d) == 0) return(NULL)
  d[d$set == naam, , drop = FALSE]
}

overzicht <- list()
for (naam in names(voorberekende_sets())) {
  termen <- voorberekende_sets()[[naam]]
  vorig <- regel_van(vorige, naam)

  # Tijdsbudget op: geen nieuwe zoekvraag starten, zodat de run publiceert
  # wat er al gelukt is in plaats van door de time-out alles kwijt te raken
  if (difftime(Sys.time(), begin, units = "mins") > TIJDBUDGET_MINUTEN) {
    message(sprintf("Zoekvraag %s overgeslagen: tijdsbudget op", naam))
    staat$mislukt <- c(staat$mislukt, sprintf("%s (tijdsbudget op)", naam))
    if (NROW(vorig) == 1) overzicht[[naam]] <- vorig
    next
  }

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
    # Een uitkomst die sterk afwijkt van de gepubliceerde versie is verdacht,
    # tenzij de vorige nacht al vrijwel dezelfde uitkomst gaf
    r <- verdacht(rij, vorig, regel_van(afgekeurd, naam))
    if (!is.null(r)) {
      nieuw_afgekeurd[[naam]] <- rij
      r <- paste0(r, "; wordt gepubliceerd als de volgende nacht hetzelfde geeft")
    }
    r
  }
  if (!is.null(reden)) {
    message(sprintf("  %s niet bijgewerkt: %s", naam, reden))
    staat$mislukt <- c(staat$mislukt, sprintf("%s (%s)", naam, reden))
    # De vorige versie blijft staan en in het overzicht
    if (NROW(vorig) == 1) overzicht[[naam]] <- vorig
    next
  }
  saveRDS(res, file.path(uitvoer, sprintf("zoek_%s.rds", rij$sleutel)))
  message(sprintf("  opgeslagen: zoek_%s.rds", rij$sleutel))
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
      invisible(file.remove(onbekend_pad))
    }
  }
}

if (length(nieuw_afgekeurd) > 0) {
  utils::write.csv(dplyr::bind_rows(nieuw_afgekeurd), afgekeurd_pad, row.names = FALSE)
} else if (file.exists(afgekeurd_pad)) {
  invisible(file.remove(afgekeurd_pad))
}

if (length(overzicht) > 0) {
  nieuw <- dplyr::bind_rows(overzicht)
  utils::write.csv(nieuw, vorig_pad, row.names = FALSE)
  # Opruimen: zoekresultaten die niet (meer) in het overzicht staan, bv. na
  # een andere methode of periode, en Iv3-bestanden van oude keuzes
  oud <- op_te_ruimen(list.files(uitvoer), nieuw$sleutel)
  if (length(oud) > 0) {
    file.remove(file.path(uitvoer, oud))
    message("Opgeruimd: ", paste(oud, collapse = ", "))
  }
}

# --- Afsluiting ---------------------------------------------------------------

# mislukt.txt laat de workflow na het publiceren rood worden, zodat een
# gedeeltelijke mislukking opvalt (de geslaagde delen zijn dan wél bijgewerkt)
waarschuwing_pad <- file.path(uitvoer, "..", "niet_gecontroleerd.txt")
if (length(staat$waarschuwingen) > 0) {
  message("Niet te controleren (vorige versie blijft): ",
          paste(staat$waarschuwingen, collapse = "; "))
  writeLines(staat$waarschuwingen, waarschuwing_pad)
} else if (file.exists(waarschuwing_pad)) {
  invisible(file.remove(waarschuwing_pad))
}

mislukt <- staat$mislukt
mislukt_pad <- file.path(uitvoer, "..", "mislukt.txt")
if (length(mislukt) > 0) {
  message("Mislukt of niet bijgewerkt: ", paste(mislukt, collapse = "; "))
  writeLines(mislukt, mislukt_pad)
} else if (file.exists(mislukt_pad)) {
  invisible(file.remove(mislukt_pad))
}
aantal_onderdelen <- 1 + length(IV3_KEUZES) + 1 + length(voorberekende_sets())
if (length(mislukt) == aantal_onderdelen) {
  stop("Alles mislukt; niets bijgewerkt.")
}
message("Klaar.")
