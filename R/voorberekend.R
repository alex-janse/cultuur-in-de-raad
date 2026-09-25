# --- Voorberekende data ------------------------------------------------------
# Een GitHub Action (scripts/voorbereken.R) rekent elke nacht de standaard-
# zoekvragen, CBS-cijfers en gemeentegrenzen uit en zet ze op de tak 'data'
# van de repository. De app leest die eerst; alleen andere zoekvragen gaan
# live naar de API. Zet de optie op NA om dit uit te schakelen.

voorberekend_url <- function() {
  getOption("cultuur.voorberekend_url",
            "https://raw.githubusercontent.com/alex-janse/cultuur-in-de-raad/data")
}

# Voorberekende zoekvragen: standaardtermen en elke themaset, met de
# standaardperiode en -opties
voorberekende_sets <- function() {
  c(list("Standaard" = STANDAARD_TERMEN), THEMASETS)
}

# Sleutel die niet afhangt van de volgorde van termen of het getaltype. De
# methode-versie zit erin, zodat een gewijzigde telmethode nooit oude
# uitkomsten oplevert.
zoek_sleutel <- function(termen, jaren, opties) {
  rlang::hash(list(
    termen = sort(unique(termen)),
    jaren = as.integer(jaren),
    opties = vapply(c("context", "woordvormen", "dedup"),
                    \(o) isTRUE(opties[[o]]), logical(1)),
    methode = methode_versie(),
    schema = SCHEMA_VERSIE
  ))
}

# Een .rds van internet mag alleen gewone gegevens bevatten: tabellen,
# lijsten, tekst, getallen en kaartvormen. Functies, environments en
# dergelijke kunnen code uitvoeren en worden geweigerd. (Het bestand komt van
# onze eigen GitHub Action; dit is een extra slot op de deur. R >= 4.4 dicht
# bovendien het bekende lek bij het inlezen van .rds-bestanden, CVE-2024-27322.)
VEILIGE_TYPES <- c("NULL", "logical", "integer", "double", "complex",
                   "character", "list")

veilig_object <- function(x, diepte = 0) {
  if (diepte > 50 || !typeof(x) %in% VEILIGE_TYPES || isS4(x)) return(FALSE)
  for (a in attributes(x)) {
    if (!veilig_object(a, diepte + 1)) return(FALSE)
  }
  if (is.list(x)) {
    for (el in x) {
      if (!veilig_object(el, diepte + 1)) return(FALSE)
    }
  }
  TRUE
}

lees_rds_veilig <- function(pad) {
  x <- readRDS(pad)
  if (veilig_object(x)) return(x)
  warning("Voorberekend bestand geweigerd: bevat meer dan gewone gegevens.")
  NULL
}

# Onthoudt per bestand de uitkomst, óók 'niet gevonden': een echte 404 een
# uur, andere fouten (time-out, storing bij GitHub) maar een minuut, zodat
# de app niet een uur lang onnodig live gaat.
voorberekend_geheugen <- new.env()

lees_voorberekend <- function(bestand) {
  basis <- voorberekend_url()
  if (is.na(basis) || !nzchar(basis)) return(NULL)
  bewaard <- voorberekend_geheugen[[bestand]]
  if (!is.null(bewaard) && Sys.time() < bewaard$geldig_tot) return(bewaard$data)

  # Een lokale map (file://) direct lezen: handig voor tests en ontwikkeling
  if (startsWith(basis, "file://")) {
    map <- sub("^file://", "", basis)                        # /tmp/x of /C:/x
    if (grepl("^/+[A-Za-z]:", map)) map <- sub("^/+", "", map)  # Windows: C:/x
    pad <- file.path(map, bestand)
    return(if (file.exists(pad)) tryCatch(lees_rds_veilig(pad), error = \(e) NULL))
  }

  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  uitkomst <- tryCatch({
    resp <- request(paste0(basis, "/", bestand)) |>
      req_timeout(20) |>
      req_error(is_error = \(r) FALSE) |>
      req_perform(path = tmp)
    if (resp_status(resp) == 200) {
      list(data = lees_rds_veilig(tmp), geldig = 3600)
    } else if (resp_status(resp) == 404) {
      list(data = NULL, geldig = 3600)
    } else {
      list(data = NULL, geldig = 60)
    }
  }, error = function(e) list(data = NULL, geldig = 60))

  voorberekend_geheugen[[bestand]] <- list(
    data = uitkomst$data, geldig_tot = Sys.time() + uitkomst$geldig)
  uitkomst$data
}

# Een voorberekend resultaat alleen gebruiken als het past bij deze versie
# van de app en niet te oud is. Anders NULL: dan wordt er live gezocht.
VERPLICHTE_VELDEN <- c("per_gemeente", "jaren_nl", "docs", "totaal_docs",
                       "termen", "jaren", "opties", "inwoners_ok",
                       "berekend_op", "schema")

bruikbaar_resultaat <- function(res) {
  is.list(res) &&
    all(VERPLICHTE_VELDEN %in% names(res)) &&
    identical(res$schema, SCHEMA_VERSIE) &&
    is.data.frame(res$per_gemeente) &&
    inherits(res$berekend_op, "POSIXct") &&
    difftime(Sys.time(), res$berekend_op, units = "days") <= MAX_LEEFTIJD_DAGEN
}

lees_zoekresultaat <- function(termen, jaren, opties) {
  res <- lees_voorberekend(sprintf("zoek_%s.rds",
                                   zoek_sleutel(termen, jaren, opties)))
  if (bruikbaar_resultaat(res)) res else NULL
}

# Voorberekend resultaat als het er is. Tussen middernacht op 1 januari en
# de nachtelijke run bestaat de standaardperiode van het nieuwe jaar nog
# niet: dan de periode t/m vorig jaar gebruiken.
zoek_voorberekend <- function(termen, jaren, opties) {
  res <- lees_zoekresultaat(termen, jaren, opties)
  if (is.null(res) && identical(as.integer(jaren), standaard_periode())) {
    res <- lees_zoekresultaat(termen, c(jaren[1], jaren[2] - 1L), opties)
  }
  if (!is.null(res)) res$bron <- "voorberekend"
  res
}

# Voorberekend als het kan, anders live (synchroon). De app gebruikt voor
# live zoeken een achtergrondtaak; deze functie is voor scripts en tests.
haal_resultaat <- function(termen, jaren, opties) {
  zoek_voorberekend(termen, jaren, opties) %||% {
    res <- haal_data_op(termen, jaren, opties)
    res$bron <- "live"
    res
  }
}

# Leeftijd van voorberekende data in dagen (voor een waarschuwing)
leeftijd_dagen <- function(res) {
  as.numeric(difftime(Sys.time(), res$berekend_op, units = "days"))
}
