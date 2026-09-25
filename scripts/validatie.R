# =============================================================================
# Validatie van de telling: klopt het dat een treffer over het onderwerp gaat?
#
# Stap 1 – steekproef trekken (per term een willekeurige selectie treffers):
#   Rscript scripts/validatie.R steekproef [aantal per term] [term ...]
#   -> validatie/steekproef_<datum>.csv  (openen in Excel, kolom 'relevant'
#      invullen met ja / nee / twijfel)
#
# Stap 2 – precisie berekenen na het invullen:
#   Rscript scripts/validatie.R bereken validatie/steekproef_<datum>.csv
#   -> precisie per term (aandeel 'ja' van ja+nee) met 95%-interval, ook
#      opgeslagen als validatie/precisie_<datum>.csv
#
# Gebruikt de standaardperiode en -opties van de app (incl. cultuurcontext).
# Eén verzoek per term, met pauzes, i.v.m. de limiet van de API.
# =============================================================================

suppressMessages(shiny::loadSupport(".", renv = globalenv()))
args <- commandArgs(trailingOnly = TRUE)
opdracht <- if (length(args) > 0) args[1] else "steekproef"
dir.create("validatie", showWarnings = FALSE)

# Willekeurige treffers voor één term, met de zin waarin de term staat
steekproef_term <- function(term, aantal, seed = 2026) {
  body <- list(
    size = aantal,
    `_source` = c("name", "last_discussed_at", "original_url", "url"),
    query = list(function_score = list(
      query = list(bool = list(filter = list(
        periode_query(standaard_periode()),
        term_query(term, STANDAARD_OPTIES)))),
      random_score = list(seed = seed, field = "_seq_no"),
      boost_mode = "replace")),
    highlight = list(
      highlight_query = term_query(term, list(context = FALSE)),
      pre_tags = list("«"), post_tags = list("»"),
      fields = list(text = list(fragment_size = 300, number_of_fragments = 1),
                    name = list(number_of_fragments = 0)))
  )
  json <- post_json(body)
  bind_rows(lapply(json$hits$hits, function(h) {
    s <- h[["_source"]]
    frag <- unlist(h$highlight$text %||% h$highlight$name %||% list(""))
    tibble(
      term = term,
      gemeente = nette_naam(ruw_naar_key(index_naar_ruw(h[["_index"]]))),
      datum = substr(as.character(s$last_discussed_at %||% ""), 1, 10),
      titel = as.character(s$name %||% ""),
      link = veilige_link(as.character(s$original_url %||% s$url %||% "")),
      fragment = gsub("\\s+", " ", paste(frag, collapse = " … ")),
      relevant = "",
      opmerking = ""
    )
  }))
}

# Wilson-interval voor een proportie (beter dan normaal bij kleine n)
wilson <- function(ja, n, z = 1.96) {
  if (n == 0) return(c(NA_real_, NA_real_))
  p <- ja / n
  midden <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  marge <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  c(midden - marge, midden + marge)
}

if (opdracht == "steekproef") {
  aantal <- if (length(args) > 1) as.integer(args[2]) else 50L
  termen <- if (length(args) > 2) args[-(1:2)] else
    unique(unlist(voorberekende_sets()))
  delen <- list()
  for (term in termen) {
    message("Steekproef: ", term)
    delen[[term]] <- tryCatch(steekproef_term(term, aantal), error = function(e) {
      message("  mislukt: ", conditionMessage(e))
      NULL
    })
    Sys.sleep(5)
  }
  uit <- bind_rows(delen) |> mutate(id = row_number(), .before = 1)
  bestand <- file.path("validatie", sprintf("steekproef_%s.csv", Sys.Date()))
  schrijf_csv(uit, bestand)
  message(sprintf("%d treffers voor %d termen -> %s", nrow(uit),
                  length(unique(uit$term)), bestand))
  message("Vul de kolom 'relevant' in met ja / nee / twijfel en draai daarna:")
  message("  Rscript scripts/validatie.R bereken ", bestand)

} else if (opdracht == "bereken") {
  bestand <- args[2]
  d <- utils::read.csv2(bestand, fileEncoding = "UTF-8-BOM",
                        stringsAsFactors = FALSE)
  d$relevant <- tolower(trimws(d$relevant))
  onbekend <- setdiff(unique(d$relevant), c("ja", "nee", "twijfel", ""))
  if (length(onbekend) > 0) {
    stop("Onbekende waarden in 'relevant': ", paste(onbekend, collapse = ", "),
         " (gebruik ja / nee / twijfel)")
  }
  uitkomst <- d |>
    group_by(term) |>
    summarise(beoordeeld = sum(relevant %in% c("ja", "nee")),
              ja = sum(relevant == "ja"),
              twijfel = sum(relevant == "twijfel"),
              leeg = sum(relevant == ""),
              .groups = "drop") |>
    rowwise() |>
    mutate(precisie = ifelse(beoordeeld > 0, ja / beoordeeld, NA_real_),
           laag = wilson(ja, beoordeeld)[1],
           hoog = wilson(ja, beoordeeld)[2]) |>
    ungroup() |>
    arrange(precisie)
  print(as.data.frame(uitkomst |> mutate(across(c(precisie, laag, hoog),
                                                \(x) round(100 * x)))))
  uit <- file.path("validatie", sprintf("precisie_%s.csv", Sys.Date()))
  schrijf_csv(uitkomst, uit)
  message("Opgeslagen: ", uit)

} else {
  stop("Onbekende opdracht: ", opdracht, " (gebruik 'steekproef' of 'bereken')")
}
