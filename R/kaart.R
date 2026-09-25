# --- Kaart: gemeenten ingekleurd naar de gekozen maatstaf ---------------------

EENHEDEN <- c(relatief = "per 1.000 raadsdocumenten",
              inwoners = "per 100.000 inwoners per jaar",
              absoluut = "documenten")

# Kolom met de waarde voor een maatstaf
maatstaf_kolom <- function(maatstaf) {
  switch(maatstaf, relatief = "per_1000", inwoners = "per_100k",
         absoluut = "totaal")
}

# Kleur voor gemeenten zonder (bruikbare) gegevens: duidelijk anders dan de
# lichtste kleur van de schaal, die een echte (lage) waarde of 0 betekent
GEEN_DATA_KLEUR <- "#b3b3b3"

# Grenzen (sf) + resultaten per gemeente -> sf met waarde en tooltip.
# Een gemeente met archief maar 0 treffers krijgt waarde 0 (een echte nul);
# alleen zonder archief of met te weinig documenten blijft het grijs.
maak_kaart_df <- function(grenzen, per_gemeente, maatstaf) {
  kolom <- maatstaf_kolom(maatstaf)
  cijfers <- if (maatstaf == "absoluut") 0 else 1
  grenzen |>
    left_join(per_gemeente |>
                mutate(waarde = .data[[kolom]], heeft_archief = TRUE) |>
                select(gemeentecode, key, gemeente, totaal, waarde,
                       weinig_treffers, heeft_archief),
              by = "gemeentecode") |>
    mutate(
      gemeente = coalesce(gemeente, grensnaam),
      naam = htmltools::htmlEscape(gemeente),
      label = case_when(
        is.na(heeft_archief) ~
          sprintf("<b>%s</b><br>geen raadsarchief in OpenBesluitvorming", naam),
        is.na(waarde) ~
          sprintf("<b>%s</b><br>te weinig documenten of inwoners voor deze maatstaf",
                  naam),
        TRUE ~
          sprintf("<b>%s</b><br>%s %s<br><small>%s documenten%s</small>",
                  naam, fmt(waarde, cijfers), EENHEDEN[[maatstaf]],
                  fmt(totaal, 0),
                  ifelse(weinig_treffers, " · te weinig voor een rang", ""))
      )
    ) |>
    select(-naam)
}

# Vlakken + legenda toevoegen aan een leaflet-kaart of -proxy
voeg_kaartlagen_toe <- function(kaart, kaart_df, maatstaf) {
  # Klassen op kwintielen, zodat uitschieters de kaart niet domineren
  breaks <- unique(quantile(kaart_df$waarde, probs = seq(0, 1, 0.2),
                            na.rm = TRUE))
  pal <- if (length(breaks) >= 2) {
    colorBin("RdPu", domain = kaart_df$waarde, bins = breaks,
             na.color = GEEN_DATA_KLEUR)
  } else {
    # Eén unieke waarde (of geen): een schaal van 0 tot die waarde
    colorNumeric("RdPu", domain = c(0, max(1, breaks)), na.color = GEEN_DATA_KLEUR)
  }
  cijfers <- if (maatstaf == "absoluut") 0 else 1
  # Legenda in Nederlandse notatie (labelFormat kent geen decimale komma)
  legenda_labels <- function(type, cuts, p) {
    if (type == "bin") {
      paste0(fmt(head(cuts, -1), cijfers), " – ", fmt(cuts[-1], cijfers))
    } else {
      fmt(cuts, cijfers)
    }
  }

  kaart |>
    addPolygons(
      data = kaart_df, layerId = ~gemeentecode,
      fillColor = ~pal(waarde), fillOpacity = 0.8,
      color = "white", weight = 0.6,
      label = lapply(kaart_df$label, htmltools::HTML),
      highlightOptions = highlightOptions(weight = 2, color = "#333",
                                          bringToFront = TRUE)
    ) |>
    addLegend("bottomright", pal = pal, values = kaart_df$waarde,
              title = EENHEDEN[[maatstaf]], opacity = 0.8,
              na.label = "geen gegevens",
              labFormat = legenda_labels)
}

basiskaart <- function() {
  leaflet() |>
    # PDOK-achtergrondkaart (Kadaster): gratis, geen API-key nodig
    addTiles(
      urlTemplate = "https://service.pdok.nl/brt/achtergrondkaart/wmts/v2_0/grijs/EPSG:3857/{z}/{x}/{y}.png",
      attribution = "Kaartgegevens &copy; <a href='https://www.kadaster.nl'>Kadaster</a>",
      options = tileOptions(minZoom = 6, maxZoom = 19)
    ) |>
    setView(lng = 5.3, lat = 52.2, zoom = 7)
}
