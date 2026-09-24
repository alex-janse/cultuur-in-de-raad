# --- Kaart: gemeenten ingekleurd naar de gekozen maatstaf ---------------------

EENHEDEN <- c(relatief = "per 1.000 raadsdocumenten",
              inwoners = "per 100.000 inwoners per jaar",
              absoluut = "documenten")

# Kolom met de waarde voor een maatstaf
maatstaf_kolom <- function(maatstaf) {
  switch(maatstaf, relatief = "per_1000", inwoners = "per_100k",
         absoluut = "totaal")
}

# Grenzen (sf) + resultaten per gemeente -> sf met waarde en tooltip
maak_kaart_df <- function(grenzen, per_gemeente, maatstaf) {
  kolom <- maatstaf_kolom(maatstaf)
  grenzen |>
    left_join(per_gemeente |>
                filter(totaal > 0) |>
                mutate(waarde = .data[[kolom]]) |>
                select(gemeentecode, key, gemeente, totaal, waarde),
              by = "gemeentecode") |>
    mutate(
      gemeente = coalesce(gemeente, grensnaam),
      label = ifelse(
        is.na(waarde),
        sprintf("<b>%s</b><br>geen (betrouwbare) gegevens",
                htmltools::htmlEscape(gemeente)),
        sprintf("<b>%s</b><br>%s %s<br><small>%s documenten</small>",
                htmltools::htmlEscape(gemeente),
                fmt(waarde, if (maatstaf == "absoluut") 0 else 1),
                EENHEDEN[[maatstaf]], fmt(totaal, 0))
      )
    )
}

# Vlakken + legenda toevoegen aan een leaflet-kaart of -proxy
voeg_kaartlagen_toe <- function(kaart, kaart_df, maatstaf) {
  # Klassen op kwintielen, zodat uitschieters de kaart niet domineren
  breaks <- unique(quantile(kaart_df$waarde, probs = seq(0, 1, 0.2),
                            na.rm = TRUE))
  pal <- if (length(breaks) >= 2) {
    colorBin("RdPu", domain = kaart_df$waarde, bins = breaks,
             na.color = "#e6e6e6")
  } else {
    # Eén unieke waarde (of geen): een schaal van 0 tot die waarde
    colorNumeric("RdPu", domain = c(0, max(1, breaks)), na.color = "#e6e6e6")
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
              na.label = "geen data",
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
