# --- Gemeentegrenzen (PDOK / CBS gebiedsindelingen) --------------------------

GEO_URL <- paste0(
  "https://service.pdok.nl/cbs/gebiedsindelingen/%d/wfs/v1_0",
  "?service=WFS&version=2.0.0&request=GetFeature",
  "&typeName=gebiedsindelingen:gemeente_gegeneraliseerd",
  "&outputFormat=application/json&srsName=EPSG:4326"
)

# Nieuwste jaar waarvoor PDOK gemeentegrenzen heeft: dit jaar, of vorig jaar
# als die van dit jaar er nog niet zijn (404). Een klein verzoek; andere
# fouten (storing) worden doorgegeven.
grenzen_jaar <- function() {
  for (jaar in c(huidig_jaar(), huidig_jaar() - 1)) {
    url <- strsplit(sprintf(GEO_URL, jaar), "?", fixed = TRUE)[[1]][1]
    resp <- request(url) |>
      req_url_query(service = "WFS", request = "GetCapabilities") |>
      req_timeout(30) |>
      req_error(is_error = \(r) FALSE) |>
      req_perform()
    if (resp_status(resp) == 200) return(jaar)
    if (resp_status(resp) != 404) stop("PDOK gaf HTTP ", resp_status(resp))
  }
  stop("PDOK heeft geen gemeentegrenzen van dit of vorig jaar.")
}

# Gemeentegrenzen van dit jaar (of vorig jaar als die er nog niet zijn),
# vereenvoudigd tot ~250 m zodat de kaart vlot laadt. 90 dagen op schijf.
# De nachtelijke run geeft het jaar mee dat grenzen_jaar() vond, zodat er
# niet stil een ouder jaar wordt opgeslagen dan is vastgelegd.
haal_gemeentegrenzen <- function(jaren = c(huidig_jaar(), huidig_jaar() - 1)) {
  schijf_cache("gemeentegrenzen", max_dagen = 90, function() {
    for (jaar in jaren) {
      bestand <- tempfile(fileext = ".geojson")
      ok <- tryCatch({
        request(sprintf(GEO_URL, jaar)) |> req_timeout(60) |>
          req_perform(path = bestand)
        TRUE
      }, error = function(e) FALSE)
      if (ok) break
    }
    if (!ok) stop("Gemeentegrenzen konden niet worden opgehaald bij PDOK.")

    sf::st_read(bestand, quiet = TRUE) |>
      sf::st_transform(28992) |>                       # RD New, in meters
      sf::st_simplify(preserveTopology = TRUE, dTolerance = 250) |>
      sf::st_transform(4326) |>
      dplyr::transmute(gemeentecode = statcode, grensnaam = statnaam)
  })
}

# Coördinaten afronden voor de kaart: 4 decimalen is ~10 m, onzichtbaar bij
# grenzen die al tot ~250 m zijn vereenvoudigd. Halveert wat leaflet naar elke
# bezoeker stuurt (~380 -> ~180 KB). Alleen voor weergave, niet om mee te rekenen.
rond_grenzen_af <- function(grenzen, decimalen = 4) {
  vormen <- lapply(sf::st_geometry(grenzen), function(vorm) {
    rapply(vorm, \(m) round(m, decimalen), how = "replace")
  })
  sf::st_geometry(grenzen) <- sf::st_sfc(vormen, crs = sf::st_crs(grenzen))
  grenzen
}
