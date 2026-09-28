# --- Ranking ------------------------------------------------------------------

# Grootteklasse op inwonertal (NA als het inwonertal onbekend is)
grootteklasse <- function(inwoners) {
  as.character(cut(inwoners, GROOTTEKLASSEN, labels = GROOTTEKLASSE_NAMEN,
                   right = FALSE))
}

# Gemeenten gerangschikt op de gekozen maatstaf, met bandbreedte (95%).
# Gemeenten met archief maar 0 treffers doen gewoon mee (een echte nul);
# gemeenten met minder dan MIN_TREFFERS_RANG treffers krijgen geen rang,
# omdat hun plek vooral toeval is. Zij staan onderaan.
# klasse: alleen gemeenten uit die grootteklasse (rang binnen de klasse).
rangschik <- function(per_gemeente, maatstaf, klasse = NULL) {
  kolom <- maatstaf_kolom(maatstaf)
  df <- per_gemeente |>
    mutate(waarde = .data[[kolom]], klasse = grootteklasse(inwoners)) |>
    filter(!is.na(waarde))
  if (!is.null(klasse) && nzchar(klasse)) {
    df <- df |> filter(.data$klasse %in% .env$klasse)
  }
  if (maatstaf == "absoluut") {
    iv <- poisson_interval(df$totaal, 1, 1)
    df$waarde_laag <- iv$laag
    df$waarde_hoog <- iv$hoog
  } else {
    df$waarde_laag <- df[[paste0(kolom, "_laag")]]
    df$waarde_hoog <- df[[paste0(kolom, "_hoog")]]
  }
  df |>
    arrange(weinig_treffers, desc(waarde)) |>
    # Gelijke waarden krijgen dezelfde rang (1, 2, 2, 4)
    mutate(rang = as.integer(min_rank(ifelse(weinig_treffers, NA, -waarde))))
}

# "rang 11 van 239", of waarom er geen rang is
rang_tekst <- function(gerangschikt, key) {
  rij <- gerangschikt[gerangschikt$key == key, ]
  if (nrow(rij) == 0) return("niet gerangschikt (te weinig gegevens)")
  if (is.na(rij$rang)) {
    return(sprintf("geen rang: minder dan %d treffers", MIN_TREFFERS_RANG))
  }
  sprintf("rang %d van %d", rij$rang, sum(!is.na(gerangschikt$rang)))
}

# Aandeel van de gerangschikte gemeenten met een lagere waarde (0-100)
percentiel <- function(gerangschikt, key) {
  metrang <- gerangschikt[!is.na(gerangschikt$rang), ]
  w <- metrang$waarde[metrang$key == key]
  if (length(w) != 1 || nrow(metrang) < 2) return(NA_real_)
  100 * sum(metrang$waarde < w) / (nrow(metrang) - 1)
}
