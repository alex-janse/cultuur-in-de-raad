# --- Nachtelijke voorberekening: controles -----------------------------------
# Gebruikt door scripts/voorbereken.R. Hier staan de beslissingen (bijwerken,
# overslaan, afkeuren), zodat ze met tests te controleren zijn; het script
# zelf regelt alleen de volgorde.

# Een nieuwe uitkomst die meer dan dit afwijkt van de gepubliceerde versie is
# verdacht. Is hij twee nachten achter elkaar ongeveer gelijk (binnen
# BEVESTIG_AFWIJKING), dan is de verandering echt en wordt hij toch gebruikt.
MAX_AFWIJKING <- 0.2
BEVESTIG_AFWIJKING <- 0.05

# Na zoveel minuten start de run geen nieuwe zoekvragen meer, zodat hij ruim
# binnen de time-out van de workflow publiceert wat er al gelukt is
TIJDBUDGET_MINUTEN <- 75

# Minimale inhoud van CBS- en PDOK-data voordat die de vorige versie vervangt
MIN_GEMEENTEN_CBS <- 300
MAX_NA_AANDEEL_CBS <- 0.5

# overzicht.csv inlezen met tekst als tekst: een methode-hash als "01234567"
# of "1234e567" zou anders een getal worden
lees_overzicht <- function(pad) {
  if (!file.exists(pad)) return(data.frame())
  d <- utils::read.csv(pad, colClasses = "character")
  for (k in intersect(c("documenten", "gemeenten"), names(d))) {
    d[[k]] <- as.integer(d[[k]])
  }
  d
}

# Reden waarom rij sterk afwijkt van v (beide één regel uit het overzicht,
# met dezelfde methode en periode), of NULL
afwijking <- function(rij, v, marge = MAX_AFWIJKING) {
  if (!is.data.frame(v) || nrow(v) != 1 ||
      !all(c("methode", "periode", "gemeenten", "documenten") %in% names(v))) {
    return(NULL)
  }
  if (!identical(v$methode, rij$methode) || !identical(v$periode, rij$periode)) {
    return(NULL)
  }
  if (rij$gemeenten < (1 - marge) * v$gemeenten) {
    return(sprintf("%d archieven, vorige keer %d", rij$gemeenten, v$gemeenten))
  }
  if (abs(rij$documenten - v$documenten) > marge * v$documenten) {
    return(sprintf("%d documenten, vorige keer %d", rij$documenten, v$documenten))
  }
  NULL
}

# Is de nieuwe uitkomst verdacht? vorig is de gepubliceerde versie, kandidaat
# de uitkomst die de vorige nacht is afgekeurd. Lijkt de nieuwe uitkomst op
# die kandidaat, dan is de verandering bevestigd en niet meer verdacht.
verdacht <- function(rij, vorig, kandidaat = NULL) {
  reden <- afwijking(rij, vorig)
  if (is.null(reden)) return(NULL)
  bevestigd <- is.data.frame(kandidaat) && nrow(kandidaat) == 1 &&
    is.null(afwijking(rij, kandidaat, BEVESTIG_AFWIJKING)) &&
    identical(kandidaat$methode, rij$methode)
  if (bevestigd) NULL else reden
}

# Reden waarom nieuw opgehaalde CBS- of PDOK-data niet goed genoeg is, of NULL
onplausibel <- function(onderdeel, data) {
  if (!is.data.frame(data)) return("geen tabel")
  if (nrow(data) < MIN_GEMEENTEN_CBS) {
    return(sprintf("%d gemeenten, minstens %d verwacht", nrow(data), MIN_GEMEENTEN_CBS))
  }
  kolom <- if (onderdeel == "inwoners") "inwoners" else
    if (onderdeel == "grenzen") "gemeentecode" else "cultuur_per_inw"
  if (!kolom %in% names(data)) return(sprintf("kolom %s ontbreekt", kolom))
  na <- mean(is.na(data[[kolom]]))
  if (na > MAX_NA_AANDEEL_CBS) {
    return(sprintf("%.0f%% van %s ontbreekt", 100 * na, kolom))
  }
  NULL
}

# Wachttijd in seconden na een mislukte poging: bij een blokkade van de API
# tot die voorbij is (anders is de volgende poging verspild), bij een 429
# vijf minuten, anders een minuut
wachttijd <- function(fout) {
  if (inherits(fout, "api_blokkade") && !is.null(blokkade_tot())) {
    return(max(0, as.numeric(difftime(blokkade_tot(), Sys.time(), units = "secs"))) + 30)
  }
  if (grepl("429", conditionMessage(fout))) 300 else 60
}

# Probeert expr_fn een paar keer. Een onvolledig antwoord van de API
# (waarschuwing uit post_json) geldt als fout: dat willen we niet bewaren.
# Andere waarschuwingen laten we gewoon door. Geeft NULL als alles mislukt.
probeer <- function(naam, expr_fn, pogingen = 3, slaap = Sys.sleep) {
  for (i in seq_len(pogingen)) {
    uitkomst <- tryCatch(
      withCallingHandlers(expr_fn(), warning = function(w) {
        # stop(w) zou een waarschuwing blijven; een nieuwe fout wordt wél
        # door tryCatch hieronder opgevangen
        if (grepl("onvolledig", conditionMessage(w))) stop(conditionMessage(w))
      }),
      error = function(e) e
    )
    if (!inherits(uitkomst, "error")) return(uitkomst)
    message(sprintf("  %s: poging %d mislukt (%s)", naam, i, conditionMessage(uitkomst)))
    if (i < pogingen) slaap(wachttijd(uitkomst))
  }
  NULL
}

# Toestand van een nachtelijke run: uitvoermap, bronversies en wat er
# mislukt of niet te controleren was
nieuwe_run <- function(uitvoer, forceer = FALSE, vandaag = format(Sys.Date()),
                       slaap = Sys.sleep) {
  pad <- file.path(uitvoer, "bronversies.rds")
  staat <- new.env()
  staat$uitvoer <- uitvoer
  staat$forceer <- forceer
  staat$vandaag <- vandaag
  staat$slaap <- slaap
  staat$versies <- if (file.exists(pad)) readRDS(pad) else leeg_bronversies()
  staat$mislukt <- character()
  staat$waarschuwingen <- character()
  staat
}

# Werkt één CBS- of PDOK-onderdeel bij als de bron een nieuwe versie heeft
# (of als het bestand ontbreekt, of bij forceren). Onbereikbaar bij de
# controle: de vorige versie blijft staan, als waarschuwing. Mislukt de
# download of is de data onplausibel: de vorige versie blijft, als fout.
# versie_fn geeft list(versie, bron_datum); haal_fn krijgt die mee.
werk_bij <- function(staat, onderdeel, bestand, versie_fn, haal_fn) {
  message(onderdeel)
  pad <- file.path(staat$uitvoer, bestand)
  rij <- which(staat$versies$onderdeel == onderdeel)
  nieuw <- probeer(paste(onderdeel, "(controle)"), versie_fn, pogingen = 2,
                   slaap = staat$slaap)
  if (is.null(nieuw)) {
    if (file.exists(pad) && !staat$forceer) {
      staat$waarschuwingen <- c(staat$waarschuwingen, onderdeel)
      message("  niet te controleren; de vorige versie blijft staan")
      return(invisible(FALSE))
    }
    nieuw <- list(versie = NA_character_, bron_datum = NA_character_)
  } else if (length(rij) == 1) {
    staat$versies$gecontroleerd[rij] <- staat$vandaag
    if (!staat$forceer && file.exists(pad) &&
        identical(staat$versies$versie[rij], nieuw$versie)) {
      message(sprintf("  ongewijzigd (%s)", nieuw$versie))
      return(invisible(FALSE))
    }
  }

  data <- probeer(onderdeel, \() haal_fn(nieuw), slaap = staat$slaap)
  reden <- if (is.null(data)) "ophalen mislukt" else onplausibel(onderdeel, data)
  if (!is.null(reden)) {
    message(sprintf("  niet bijgewerkt: %s", reden))
    staat$mislukt <- c(staat$mislukt, sprintf("%s (%s)", onderdeel, reden))
    return(invisible(FALSE))
  }
  saveRDS(data, pad)
  message(sprintf("  opgeslagen: %s", bestand))
  regel <- data.frame(onderdeel = onderdeel, versie = nieuw$versie,
                      bron_datum = nieuw$bron_datum, opgehaald = staat$vandaag,
                      gecontroleerd = if (is.na(nieuw$versie)) NA_character_ else staat$vandaag)
  staat$versies <- rbind(
    staat$versies[staat$versies$onderdeel != onderdeel, , drop = FALSE], regel)
  invisible(TRUE)
}

# Bestanden in de uitvoermap die weg kunnen: zoekresultaten die niet in het
# overzicht staan en Iv3-bestanden van keuzes die niet meer bestaan
op_te_ruimen <- function(bestanden, sleutels, keuzes = IV3_KEUZES) {
  zoek <- grep("^zoek_.*[.]rds$", bestanden, value = TRUE)
  iv3 <- grep("^iv3_.*[.]rds$", bestanden, value = TRUE)
  c(setdiff(zoek, sprintf("zoek_%s.rds", sleutels)),
    setdiff(iv3, sprintf("iv3_%s.rds", keuzes)))
}

# Een periode in delen voor de nachtelijke run: c(2010, 2026) met lengte 8
# geeft 2010-2017, 2018-2025 en 2026-2026 (zie PERIODE_DEEL_JAREN)
periode_delen <- function(jaren, lengte = PERIODE_DEEL_JAREN) {
  begin <- seq(as.integer(jaren[1]), as.integer(jaren[2]), by = lengte)
  lapply(begin, \(b) c(b, min(b + as.integer(lengte) - 1L, as.integer(jaren[2]))))
}

# De resultaten van de delen samenvoegen tot één resultaat over alle jaren.
# De cijfers per jaar komen uit de delen; de totalen per gemeente worden
# daaruit opnieuw opgeteld, net zoals de app een periodeblok maakt.
voeg_delen_samen <- function(delen, jaren) {
  vast <- bind_rows(lapply(delen, `[[`, "per_gemeente")) |>
    select(key, any_of(c("ruw", "gemeentecode", "cbs_naam", "inwoners",
                         "inwoners_jaar", "gemeente"))) |>
    group_by(key) |>
    summarise(ruw = list(unique(unlist(ruw))),
              across(-ruw, first), .groups = "drop")
  trends <- bind_rows(lapply(delen, `[[`, "trends"))

  res <- delen[[length(delen)]]
  res$trends <- trends
  res$jaren_nl <- trends |>
    group_by(jaar, term) |>
    summarise(n = sum(n), archief = sum(archief), .groups = "drop")
  res$per_gemeente <- vast
  res$docs <- bind_rows(lapply(delen, `[[`, "docs")) |>
    arrange(desc(datum)) |>
    head(MAX_DOCS)
  res$inwoners_ok <- all(vapply(delen, \(d) isTRUE(d$inwoners_ok), logical(1)))
  res$jaren <- as.integer(jaren)

  samen <- blok_resultaat(res, jaren)
  samen$periode_volledig <- NULL
  samen
}
