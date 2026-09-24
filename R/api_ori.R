# --- OpenBesluitvorming-API --------------------------------------------------

# Exacte-woordgroep-match op titel, beschrijving en volledige tekst.
term_query <- function(term) {
  list(multi_match = list(query = term, type = "phrase",
                          fields = c("name", "description", "text")))
}
of_query <- function(termen) {
  list(bool = list(should = lapply(termen, term_query),
                   minimum_should_match = 1))
}

# Filters: één bucket voor 'minstens één term' plus één per term
treffer_filters <- function(termen) {
  list(filters = list(filters = c(
    list(`__alle__` = of_query(termen)),
    setNames(lapply(termen, term_query), termen)
  )))
}

# Sommige documenten hebben een datum in de toekomst (bv. 31-12 als
# placeholder); die laten we weg door nooit verder dan vandaag te kijken.
periode_query <- function(jaren) {
  tot <- if (jaren[2] >= HUIDIG_JAAR) "now+1d/d" else sprintf("%d-01-01", jaren[2] + 1)
  list(bool = list(filter = list(list(range = list(last_discussed_at = list(
    gte = sprintf("%d-01-01", jaren[1]),
    lt  = tot
  ))))))
}

jaren_agg <- function(termen) {
  list(date_histogram = list(field = "last_discussed_at",
                             calendar_interval = "year", min_doc_count = 0),
       aggs = list(t = treffer_filters(termen)))
}

post_json <- function(body, index = "ori_*") {
  resp <- request(API_BASIS) |>
    req_url_path_append(index, "_search") |>
    req_body_json(body) |>
    req_timeout(90) |>
    req_user_agent("cultuur-dashboard-mvp (R/httr2)") |>
    req_error(is_error = function(r) FALSE) |>
    req_perform()

  if (resp_status(resp) >= 400) {
    stop(sprintf("API gaf HTTP %s terug.", resp_status(resp)))
  }
  tekst <- resp_body_string(resp)
  if (!nzchar(trimws(tekst))) stop("API gaf een lege respons terug.")
  fromJSON(tekst, simplifyVector = FALSE)
}

# Aantallen uit een 'filters'-bucket -> named integer vector
bucket_tellingen <- function(b, termen) {
  vapply(c("__alle__", termen), function(t) {
    as.integer(b$t$buckets[[t]]$doc_count %||% 0L)
  }, integer(1))
}

# date_histogram-buckets -> lange tabel (jaar, term, n, archief)
parse_jaren <- function(buckets, termen) {
  if (length(buckets) == 0) {
    return(tibble(jaar = integer(), term = character(),
                  n = integer(), archief = numeric()))
  }
  bind_rows(lapply(buckets, function(b) {
    n <- bucket_tellingen(b, termen)
    tibble(jaar = as.integer(substr(b$key_as_string, 1, 4)),
           term = names(n), n = unname(n),
           archief = as.numeric(b$doc_count))
  }))
}

# Eén request: archiefgrootte + treffers per gemeente en per jaar, en de
# 100 nieuwste treffers (post_filter raakt alleen de hits, niet de aggregaties).
haal_data_op <- function(termen, jaren) {
  body <- list(
    size = MAX_DOCS,
    track_total_hits = TRUE,
    `_source` = c("name", "last_discussed_at", "original_url", "url"),
    sort = list(list(last_discussed_at = "desc")),
    query = periode_query(jaren),
    post_filter = of_query(termen),
    aggs = list(
      gemeenten = list(terms = list(field = "_index", size = 1000),
                       aggs = list(t = treffer_filters(termen))),
      jaren = jaren_agg(termen)
    )
  )
  json <- post_json(body)

  g_buckets <- json$aggregations$gemeenten$buckets
  j_buckets <- json$aggregations$jaren$buckets
  if (is.null(g_buckets) || is.null(j_buckets) || is.null(json$hits)) {
    stop("Onverwachte JSON-structuur (geen 'aggregations' of 'hits').")
  }
  if (length(g_buckets) == 0) stop("Geen documenten gevonden in deze periode.")

  per_gemeente <- bind_rows(lapply(g_buckets, function(b) {
    n <- bucket_tellingen(b, termen)
    ruw <- index_naar_ruw(b$key)
    tibble(key = ruw_naar_key(ruw), ruw = ruw,
           archief = as.numeric(b$doc_count), totaal = n[["__alle__"]],
           !!!as.list(n[termen]))
  })) |>
    group_by(key) |>
    summarise(ruw = list(unique(ruw)),
              across(c(archief, totaal, all_of(termen)), sum),
              .groups = "drop")

  inwoners <- tryCatch(haal_inwoners(), error = function(e) {
    warning("CBS-inwoners niet beschikbaar: ", conditionMessage(e))
    tibble(key = character(), gemeentecode = character(),
           cbs_naam = character(), inwoners = numeric(),
           inwoners_jaar = character())
  })

  n_jaren <- jaren[2] - jaren[1] + 1
  per_gemeente <- per_gemeente |>
    left_join(inwoners, by = "key") |>
    left_join(GEMEENTEN, by = "key") |>
    mutate(
      gemeente = coalesce(gemeente, cbs_naam, nette_naam(key)),
      per_1000 = ifelse(archief >= MIN_DOCS_PER_JAAR * n_jaren,
                        1000 * totaal / archief, NA_real_),
      per_100k = ifelse(inwoners >= MIN_INWONERS,
                        1e5 * totaal / inwoners / n_jaren, NA_real_)
    ) |>
    arrange(desc(totaal))

  docs <- bind_rows(lapply(json$hits$hits, function(h) {
    s <- h[["_source"]]
    key <- ruw_naar_key(index_naar_ruw(h[["_index"]]))
    tibble(
      key   = key,
      titel = as.character(s$name %||% "(zonder titel)"),
      datum = substr(as.character(s$last_discussed_at %||% ""), 1, 10),
      link  = as.character(s$original_url %||% s$url %||% "")
    )
  }))
  if (nrow(docs) > 0) {
    docs <- docs |>
      left_join(per_gemeente |> select(key, gemeente), by = "key")
  }

  list(
    per_gemeente = per_gemeente,
    jaren_nl = parse_jaren(j_buckets, termen),
    docs = docs,
    totaal_docs = json$hits$total$value %||% sum(per_gemeente$totaal),
    termen = termen,
    jaren = jaren,
    inwoners_ok = nrow(inwoners) > 0
  )
}

# Trend voor één gemeente (alleen de indexen van die gemeente doorzoeken)
haal_trend_gemeente <- function(ruwe_namen, termen, jaren) {
  body <- list(size = 0, query = periode_query(jaren),
               aggs = list(jaren = jaren_agg(termen)))
  json <- post_json(body, index = index_patroon(ruwe_namen))
  buckets <- json$aggregations$jaren$buckets
  if (is.null(buckets)) stop("Onverwachte JSON-structuur voor trend.")
  parse_jaren(buckets, termen)
}
