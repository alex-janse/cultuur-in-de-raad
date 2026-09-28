# --- CBS: inwoners per gemeente ----------------------------------------------

# Verandert hooguit jaarlijks: 30 dagen op schijf bewaren.
haal_inwoners <- function() {
  schijf_cache("cbs_inwoners", max_dagen = 30, function() {
    meta <- cbs_get_meta(CBS_TABEL_BEVOLKING)
    perioden <- tail(grep("JJ00$", meta$Perioden$Key, value = TRUE), 2)
    d <- cbs_get_data(CBS_TABEL_BEVOLKING, Perioden = perioden,
                      RegioS = has_substring("GM"),
                      select = c("RegioS", "Perioden", "TotaleBevolking_1"))
    if (nrow(d) == 0) stop("CBS gaf geen bevolkingsdata terug.")

    d |>
      filter(!is.na(TotaleBevolking_1)) |>
      group_by(RegioS) |>
      slice_max(Perioden, n = 1) |>
      ungroup() |>
      left_join(meta$RegioS |> select(Key, cbs_naam = Title),
                by = c("RegioS" = "Key")) |>
      mutate(
        gemeentecode = trimws(RegioS),
        cbs_naam = trimws(cbs_naam),
        key = coalesce(unname(CBS_NAAM_NAAR_KEY[cbs_naam]), naar_key(cbs_naam)),
        cbs_naam = coalesce(unname(WEERGAVENAAM[key]),
                            sub(" [(]gemeente[)]$", "", cbs_naam)),
        inwoners = as.numeric(TotaleBevolking_1),
        inwoners_jaar = substr(Perioden, 1, 4)
      ) |>
      select(key, gemeentecode, cbs_naam, inwoners, inwoners_jaar)
  })
}

# --- CBS: gemeentelijke lasten voor cultuur (Iv3) -----------------------------

# keuze: "2024_rekening" of "2026_begroting" (zie iv3_keuzes()); tabel: de
# CBS-tabel van dat jaar (de nachtelijke run geeft die mee uit de catalogus)
haal_cultuurlasten <- function(keuze = "2024_rekening", tabel = NULL) {
  delen <- strsplit(keuze, "_")[[1]]
  jaar <- as.integer(delen[1])
  verslagsoort <- if (delen[2] == "rekening") "X005" else "X000"

  schijf_cache(paste0("iv3_", keuze), max_dagen = 30, function() {
    tabel <- tabel %||% iv3_tabel(jaar)
    basis <- sprintf("https://dataderden.cbs.nl/ODataFeed/odata/%s", tabel)
    # Sleutels zijn met spaties aangevuld tot 6 tekens: '5.3   '
    filter <- sprintf(
      "Verslagsoort eq '%s%s' and startswith(Categorie,'L') and (%s)",
      jaar, verslagsoort,
      paste(sprintf("TaakveldBalanspost eq '%-6s'", IV3_TAAKVELDEN),
            collapse = " or "))

    req <- request(paste0(basis, "/TypedDataSet")) |>
      req_url_query(`$filter` = filter,
                    `$select` = "TaakveldBalanspost,Gemeenten,k_1ePlaatsing_1") |>
      req_headers(Accept = "application/json") |>  # anders Atom-XML
      req_timeout(90)

    rijen <- list()
    repeat {  # de feed pagineert per 10.000 rijen
      js <- req |> req_retry(max_tries = 3) |> req_perform() |>
        resp_body_json(simplifyVector = TRUE)
      if (is.null(js$value)) stop("Onverwachte JSON-structuur van CBS Iv3.")
      rijen[[length(rijen) + 1]] <- js$value
      volgende <- js[["odata.nextLink"]]
      if (is.null(volgende)) break
      req <- request(volgende) |> req_headers(Accept = "application/json") |>
        req_timeout(90)
    }
    if (nrow(bind_rows(rijen)) == 0) {
      stop(sprintf("CBS heeft (nog) geen Iv3-data voor %s.", keuze))
    }

    inw_jaar <- min(jaar, huidig_jaar() - 1)
    inwoners <- cbs_get_data(
      CBS_TABEL_BEVOLKING_IV3, Perioden = paste0(inw_jaar, "JJ00"), Geslacht = "T001038",
      Leeftijd = "10000", BurgerlijkeStaat = "T001019",
      RegioS = has_substring("GM")
    ) |>
      transmute(gemeentecode = trimws(RegioS),
                inwoners_iv3 = as.numeric(BevolkingOp1Januari_1))

    bind_rows(rijen) |>
      mutate(gemeentecode = trimws(Gemeenten)) |>
      group_by(gemeentecode) |>
      summarise(lasten_keur = sum(k_1ePlaatsing_1, na.rm = TRUE),
                .groups = "drop") |>
      left_join(inwoners, by = "gemeentecode") |>
      transmute(gemeentecode, keuze = keuze, jaar = jaar,
                # 0 betekent: (nog) niet aangeleverd
                cultuur_per_inw = ifelse(lasten_keur > 0,
                                         lasten_keur * 1000 / inwoners_iv3,
                                         NA_real_))
  })
}

# --- CBS: is er iets nieuws? --------------------------------------------------

# Datum (jjjj-mm-dd) waarop CBS een tabel voor het laatst heeft gewijzigd. Een
# klein verzoek; zo downloadt de nachtelijke run alleen als er iets nieuws is.
cbs_gewijzigd <- function(tabel, basis = "https://opendata.cbs.nl") {
  js <- request(sprintf("%s/ODataApi/odata/%s/TableInfos", basis, tabel)) |>
    req_url_query(`$select` = "Modified") |>
    req_headers(Accept = "application/json") |>
    req_timeout(30) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
  datum <- js$value$Modified
  if (length(datum) != 1 || is.na(datum)) {
    stop("CBS gaf geen wijzigingsdatum voor tabel ", tabel)
  }
  substr(datum, 1, 10)
}

# Versie van een onderdeel ("inwoners" of een Iv3-keuze): de wijzigingsdatum
# van elke tabel die het gebruikt. bron_datum is die van de hoofdtabel.
cbs_versie <- function(onderdeel, tabel = NULL) {
  tabellen <- if (onderdeel == "inwoners") {
    list(c(CBS_TABEL_BEVOLKING, "https://opendata.cbs.nl"))
  } else {
    jaar <- strsplit(onderdeel, "_")[[1]][1]
    list(c(tabel %||% iv3_tabel(jaar), "https://dataderden.cbs.nl"),
         c(CBS_TABEL_BEVOLKING_IV3, "https://opendata.cbs.nl"))
  }
  data <- vapply(tabellen, \(t) cbs_gewijzigd(t[1], t[2]), character(1))
  list(versie = paste(vapply(tabellen, `[`, "", 1), data, collapse = ", "),
       bron_datum = data[1])
}

# --- CBS: welke Iv3-tabellen en -keuzes zijn er? -------------------------------

IV3_BASIS <- "https://dataderden.cbs.nl"

# Jaar -> tabel, uit de catalogus van dataderden.cbs.nl (één verzoek)
iv3_catalogus <- function() {
  js <- request(paste0(IV3_BASIS, "/ODataCatalog/Tables")) |>
    req_url_query(`$select` = "Identifier,Title", `$format` = "json") |>
    req_timeout(60) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
  t <- js$value
  t <- t[grepl("^Gemeenten [0-9]{4} onbewerkte Iv3-data *$", t$Title), , drop = FALSE]
  if (NROW(t) == 0) stop("Geen Iv3-tabellen voor gemeenten in de CBS-catalogus.")
  setNames(t$Identifier, sub("^Gemeenten ([0-9]{4}).*$", "\\1", t$Title))
}

# Hoeveel gemeenten hebben voor taakveld 5.3 (cultuur) al een bedrag in deze
# verslagsoort? Kijkt naar de eerste pagina (10.000 rijen, ~0,6 MB): genoeg om
# 'niets' van 'vrijwel alle gemeenten' te onderscheiden. CBS negeert een filter
# op het bedrag zelf, dus dat tellen we hier.
iv3_gemeenten_met_data <- function(tabel, jaar, soort) {
  code <- if (soort == "rekening") "X005" else "X000"
  js <- request(sprintf("%s/ODataFeed/odata/%s/TypedDataSet", IV3_BASIS, tabel)) |>
    req_url_query(
      `$filter` = sprintf("Verslagsoort eq '%s%s' and startswith(Categorie,'L') and TaakveldBalanspost eq '5.3   '",
                          jaar, code),
      `$select` = "Gemeenten,k_1ePlaatsing_1") |>
    req_headers(Accept = "application/json") |>
    req_timeout(90) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
  v <- js$value
  if (NROW(v) == 0) return(0L)
  length(unique(v$Gemeenten[!is.na(v$k_1ePlaatsing_1) & v$k_1ePlaatsing_1 != 0]))
}

# De budgetkeuzes voor de app: per soort (eerst jaarrekeningen, dan
# begrotingen) de 'aantal' nieuwste jaren met cijfers van genoeg gemeenten.
ontdek_iv3_keuzes <- function(aantal = IV3_AANTAL_JAREN, catalogus = iv3_catalogus(),
                              met_data = iv3_gemeenten_met_data) {
  jaren <- sort(as.integer(names(catalogus)), decreasing = TRUE)
  rijen <- list()
  for (soort in c("rekening", "begroting")) {
    gevonden <- 0
    for (jaar in jaren) {
      if (gevonden >= aantal) break
      tabel <- unname(catalogus[as.character(jaar)])
      if (met_data(tabel, jaar, soort) < IV3_MIN_GEMEENTEN) next
      rijen[[length(rijen) + 1]] <- data.frame(
        keuze = sprintf("%d_%s", jaar, soort),
        label = sprintf("%s %d", if (soort == "rekening") "Jaarrekening" else "Begroting", jaar),
        jaar = jaar, soort = soort, tabel = tabel)
      gevonden <- gevonden + 1
    }
  }
  if (length(rijen) == 0) stop("Geen Iv3-jaren met cijfers gevonden.")
  do.call(rbind, rijen)
}

# Beschikbare budgetkeuzes: uit de nachtelijke run (iv3_keuzes.rds), anders
# de reservelijst IV3_KEUZES
iv3_keuzes_tabel <- function() {
  k <- lees_voorberekend("iv3_keuzes.rds")
  if (is.data.frame(k) && NROW(k) > 0 &&
      all(c("keuze", "label", "jaar", "tabel") %in% names(k))) {
    return(k)
  }
  jaar <- as.integer(substr(IV3_KEUZES, 1, 4))
  data.frame(keuze = unname(IV3_KEUZES), label = names(IV3_KEUZES), jaar = jaar,
             soort = sub("^[0-9]+_", "", IV3_KEUZES),
             tabel = unname(IV3_TABELLEN[as.character(jaar)]))
}

# Als benoemde vector (label = keuze), zoals selectInput die wil
iv3_keuzes <- function() {
  k <- iv3_keuzes_tabel()
  setNames(k$keuze, k$label)
}

iv3_tabel <- function(jaar) {
  k <- iv3_keuzes_tabel()
  tabel <- k$tabel[k$jaar == jaar][1]
  if (is.na(tabel)) tabel <- unname(IV3_TABELLEN[as.character(jaar)])
  if (is.na(tabel)) stop("Geen Iv3-tabel bekend voor ", jaar)
  tabel
}
