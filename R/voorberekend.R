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
VOORBEREKEND_JAREN <- c(2020L, HUIDIG_JAAR)
voorberekende_sets <- function() {
  c(list("Standaard" = STANDAARD_TERMEN), THEMASETS)
}

# Sleutel die niet afhangt van de volgorde van termen of het getaltype
zoek_sleutel <- function(termen, jaren, opties) {
  rlang::hash(list(
    termen = sort(unique(termen)),
    jaren = as.integer(jaren),
    opties = vapply(c("context", "woordvormen", "dedup"),
                    \(o) isTRUE(opties[[o]]), logical(1))
  ))
}

voorberekend_cache <- cachem::cache_mem(max_age = 3600)

# Leest <bestand> van de data-tak; NULL als het er niet is of niet lukt
lees_voorberekend <- function(bestand) {
  basis <- voorberekend_url()
  if (is.na(basis) || !nzchar(basis)) return(NULL)
  sleutel <- gsub("[^a-z0-9]", "", tolower(bestand))
  bewaard <- voorberekend_cache$get(sleutel)
  if (!cachem::is.key_missing(bewaard)) return(bewaard)

  data <- tryCatch({
    tmp <- tempfile(fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    request(paste0(basis, "/", bestand)) |>
      req_timeout(20) |>
      req_perform(path = tmp)
    readRDS(tmp)
  }, error = function(e) NULL)
  voorberekend_cache$set(sleutel, data)  # ook 'niet gevonden' onthouden
  data
}

# Voorberekend resultaat als het er is, anders live bij de API
haal_resultaat <- function(termen, jaren, opties) {
  res <- lees_voorberekend(sprintf("zoek_%s.rds",
                                   zoek_sleutel(termen, jaren, opties)))
  if (!is.null(res)) {
    res$bron <- "voorberekend"
    return(res)
  }
  res <- haal_data_op(termen, jaren, opties)
  res$bron <- "live"
  res
}
