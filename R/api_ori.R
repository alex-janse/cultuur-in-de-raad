# --- OpenBesluitvorming-API --------------------------------------------------

# Zoekopties (schakelaars in de UI):
#   context     – term moet in de buurt van een cultuurwoord staan
#   woordvormen – 'amateurkunst' vindt ook 'amateurkunstenaars'
#   dedup       – identieke bijlagen (zelfde bestandsgrootte) één keer tellen
STANDAARD_OPTIES <- list(context = TRUE, woordvormen = FALSE, dedup = TRUE)

VELDEN <- c("name", "description", "text")

# Speciale tekens van de query_string-syntax onschadelijk maken
escape_qs <- function(x) {
  gsub('([-+=&|><!(){}\\[\\]^"~*?:\\\\/])', "\\\\\\1", x, perl = TRUE)
}

context_nodig <- function(term, opties) {
  isTRUE(opties$context) && !grepl(CULTUUR_REGEX, term)
}

term_query <- function(term, opties = STANDAARD_OPTIES) {
  enkel_woord <- !grepl("\\s", term)
  woordvorm <- isTRUE(opties$woordvormen) && enkel_woord

  if (context_nodig(term, opties)) {
    # 'intervals' zoekt binnen één veld; de volledige tekst is het relevante
    kern <- if (woordvorm) {
      list(prefix = list(prefix = term))
    } else {
      list(match = list(query = term, ordered = TRUE, max_gaps = 0))
    }
    cultuur <- lapply(CULTUURWOORDEN, \(w) list(match = list(query = w)))
    return(list(intervals = list(text = list(all_of = list(
      ordered = FALSE, max_gaps = CONTEXT_AFSTAND,
      intervals = list(kern, list(any_of = list(intervals = cultuur)))
    )))))
  }

  if (woordvorm) {
    list(query_string = list(query = paste0(escape_qs(term), "*"),
                             fields = VELDEN))
  } else {
    list(multi_match = list(query = term, type = "phrase", fields = VELDEN))
  }
}

of_query <- function(termen, opties = STANDAARD_OPTIES) {
  list(bool = list(should = lapply(termen, term_query, opties = opties),
                   minimum_should_match = 1))
}

# Sub-aggregaties om dubbele bijlagen weg te filteren. Een bijlage die bij
# meerdere agendapunten hangt heeft steeds dezelfde bestandsgrootte; de
# bestandsnaam is geen goede sleutel ('Bijlage 1.pdf'). Documenten zonder
# bestand (agendapunten, vergaderingen) tellen we apart gewoon mee.
telling_aggs <- function(dedup) {
  if (!isTRUE(dedup)) return(NULL)
  list(
    uniek = list(cardinality = list(field = "size_in_bytes",
                                    precision_threshold = 40000)),
    zonder = list(filter = list(bool = list(must_not = list(
      exists = list(field = "size_in_bytes")))))
  )
}

# Aantal documenten in een bucket, met of zonder samenvoegen van duplicaten
tel <- function(b, dedup) {
  if (is.null(b)) return(0)
  if (isTRUE(dedup) && !is.null(b$uniek)) {
    as.numeric(b$uniek$value %||% 0) + as.numeric(b$zonder$doc_count %||% 0)
  } else {
    as.numeric(b$doc_count %||% 0)
  }
}

# Filters: één bucket voor 'minstens één term' plus één per term
treffer_filters <- function(termen, opties = STANDAARD_OPTIES) {
  agg <- list(filters = list(filters = c(
    list(`__alle__` = of_query(termen, opties)),
    setNames(lapply(termen, term_query, opties = opties), termen)
  )))
  sub <- telling_aggs(opties$dedup)
  if (!is.null(sub)) agg$aggs <- sub
  agg
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

jaren_agg <- function(termen, opties = STANDAARD_OPTIES) {
  list(date_histogram = list(field = "last_discussed_at",
                             calendar_interval = "year", min_doc_count = 0),
       aggs = c(list(t = treffer_filters(termen, opties)),
                telling_aggs(opties$dedup)))
}

post_json <- function(body, index = "ori_*", pogingen = 2) {
  sleutel <- rlang::hash(list(index, body))
  bewaard <- ori_cache$get(sleutel)
  if (!cachem::is.key_missing(bewaard)) return(bewaard)

  for (poging in seq_len(pogingen)) {
    resp <- request(API_BASIS) |>
      req_url_path_append(index, "_search") |>
      req_body_json(body) |>
      req_timeout(90) |>
      req_user_agent("cultuur-dashboard-mvp (R/httr2)") |>
      req_retry(max_tries = 4, backoff = \(i) 2 * 2^i,
                is_transient = \(r) resp_status(r) %in% c(429, 502, 503, 504)) |>
      req_error(is_error = function(r) FALSE) |>
      req_perform()

    if (resp_status(resp) == 429) {
      stop("De API krijgt even te veel verzoeken (HTTP 429). ",
           "Probeer het over een minuut opnieuw.")
    }
    if (resp_status(resp) >= 400) {
      stop(sprintf("API gaf HTTP %s terug.", resp_status(resp)))
    }
    tekst <- resp_body_string(resp)
    if (!nzchar(trimws(tekst))) stop("API gaf een lege respons terug.")
    json <- fromJSON(tekst, simplifyVector = FALSE)

    # De server laat soms tijdelijk archiefdelen vallen: één keer opnieuw
    if ((json$`_shards`$failed %||% 0) == 0) break
  }

  # Blijvend mislukt, bv. een te korte term met woordvormen ('kunst*') die
  # de expansielimiet overschrijdt: dan doet een deel van het archief niet mee.
  mislukt <- json$`_shards`$failed %||% 0
  if (mislukt > 0) {
    warning(sprintf(paste(
      "%d van de %d archiefdelen konden niet worden doorzocht; de cijfers",
      "zijn onvolledig. Probeer een langere term of zet 'woordvormen' uit."),
      mislukt, json$`_shards`$total %||% NA))
  } else {
    ori_cache$set(sleutel, json)  # onvolledige antwoorden niet bewaren
  }
  json
}

# Aantallen uit een 'filters'-bucket -> named numeric vector
bucket_tellingen <- function(b, termen, dedup = FALSE) {
  vapply(c("__alle__", termen), function(t) tel(b$t$buckets[[t]], dedup),
         numeric(1))
}

# date_histogram-buckets -> lange tabel (jaar, term, n, archief)
parse_jaren <- function(buckets, termen, dedup = FALSE) {
  if (length(buckets) == 0) {
    return(tibble(jaar = integer(), term = character(),
                  n = numeric(), archief = numeric()))
  }
  bind_rows(lapply(buckets, function(b) {
    n <- bucket_tellingen(b, termen, dedup)
    tibble(jaar = as.integer(substr(b$key_as_string, 1, 4)),
           term = names(n), n = unname(n),
           archief = tel(b, dedup))
  }))
}

# Eén request: archiefgrootte + treffers per gemeente en per jaar, en de
# 100 nieuwste treffers (post_filter raakt alleen de hits, niet de aggregaties).
haal_data_op <- function(termen, jaren, opties = STANDAARD_OPTIES) {
  body <- list(
    size = MAX_DOCS,
    track_total_hits = TRUE,
    `_source` = c("name", "last_discussed_at", "original_url", "url"),
    sort = list(list(last_discussed_at = "desc")),
    query = periode_query(jaren),
    post_filter = of_query(termen, opties),
    aggs = list(
      gemeenten = list(terms = list(field = "_index", size = 1000),
                       aggs = c(list(t = treffer_filters(termen, opties)),
                                telling_aggs(opties$dedup))),
      jaren = jaren_agg(termen, opties)
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
    n <- bucket_tellingen(b, termen, opties$dedup)
    ruw <- index_naar_ruw(b$key)
    tibble(key = ruw_naar_key(ruw), ruw = ruw,
           archief = tel(b, opties$dedup), totaal = n[["__alle__"]],
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
    mutate(
      gemeente = coalesce(cbs_naam, nette_naam(key)),
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
    jaren_nl = parse_jaren(j_buckets, termen, opties$dedup),
    docs = docs,
    totaal_docs = sum(per_gemeente$totaal),
    termen = termen,
    jaren = jaren,
    opties = opties,
    inwoners_ok = nrow(inwoners) > 0
  )
}

# Trend voor één gemeente (alleen de indexen van die gemeente doorzoeken)
haal_trend_gemeente <- function(ruwe_namen, termen, jaren,
                                opties = STANDAARD_OPTIES) {
  body <- list(size = 0, query = periode_query(jaren),
               aggs = list(jaren = jaren_agg(termen, opties)))
  json <- post_json(body, index = index_patroon(ruwe_namen))
  buckets <- json$aggregations$jaren$buckets
  if (is.null(buckets)) stop("Onverwachte JSON-structuur voor trend.")
  parse_jaren(buckets, termen, opties$dedup)
}

# Trends van álle gemeenten in één verzoek (gemeente > jaar > term). Zwaar
# (~10 s, ~15 MB JSON), daarom alleen voor de nachtelijke voorberekening.
haal_trends_alle <- function(termen, jaren, opties = STANDAARD_OPTIES) {
  body <- list(size = 0, query = periode_query(jaren),
               aggs = list(gemeenten = list(
                 terms = list(field = "_index", size = 1000),
                 aggs = list(jaren = jaren_agg(termen, opties)))))
  json <- post_json(body)
  buckets <- json$aggregations$gemeenten$buckets
  if (is.null(buckets)) stop("Onverwachte JSON-structuur voor trends.")

  bind_rows(lapply(buckets, function(b) {
    parse_jaren(b$jaren$buckets, termen, opties$dedup) |>
      mutate(key = ruw_naar_key(index_naar_ruw(b$key)))
  })) |>
    # stadsdelen en fusiegemeenten optellen bij de huidige gemeente
    group_by(key, jaar, term) |>
    summarise(n = sum(n), archief = sum(archief), .groups = "drop")
}

# Nieuwste treffers van één gemeente met de zinnen waarin een term voorkomt.
# De markering gebruikt een eenvoudige zoekvraag op de termen zelf: de
# 'intervals'-query van de cultuurcontext markeert anders ook de cultuurwoorden.
haal_fragmenten <- function(ruwe_namen, termen, jaren, opties = STANDAARD_OPTIES,
                            aantal = 20) {
  markeer <- of_query(termen, modifyList(opties, list(context = FALSE)))
  body <- list(
    size = aantal,
    `_source` = c("name", "last_discussed_at", "original_url", "url"),
    sort = list(list(last_discussed_at = "desc")),
    query = list(bool = list(filter = list(periode_query(jaren),
                                           of_query(termen, opties)))),
    highlight = list(
      highlight_query = markeer,
      encoder = "html",  # tekst escapen, alleen onze <mark>-tags blijven
      pre_tags = list("<mark>"), post_tags = list("</mark>"),
      fields = list(text = list(fragment_size = 220, number_of_fragments = 2),
                    name = list(number_of_fragments = 0))
    )
  )
  json <- post_json(body, index = index_patroon(ruwe_namen))
  if (is.null(json$hits$hits)) stop("Onverwachte JSON-structuur voor fragmenten.")

  bind_rows(lapply(json$hits$hits, function(h) {
    s <- h[["_source"]]
    frag <- unlist(h$highlight$text %||% list())
    tibble(
      titel = as.character(s$name %||% "(zonder titel)"),
      datum = substr(as.character(s$last_discussed_at %||% ""), 1, 10),
      link = as.character(s$original_url %||% s$url %||% ""),
      # HTML-veilig: door ES ge-escaped, met alleen <mark> van ons
      fragmenten = paste(gsub("\\s+", " ", frag), collapse = " … ")
    )
  }))
}
