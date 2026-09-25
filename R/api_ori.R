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

is_cultuurterm <- function(term) {
  woorden <- strsplit(tolower(term), "[^[:alnum:]]+")[[1]]
  term %in% CULTUURWOORDEN ||
    any(grepl(CULTUUR_STAMMEN, woorden) & !grepl(GEEN_CULTUUR, woorden))
}

context_nodig <- function(term, opties) {
  isTRUE(opties$context) && !is_cultuurterm(term)
}

term_query <- function(term, opties = STANDAARD_OPTIES) {
  enkel_woord <- !grepl("\\s", term)
  woordvorm <- isTRUE(opties$woordvormen) && enkel_woord &&
    nchar(term) >= MIN_TEKENS_WOORDVORMEN

  if (context_nodig(term, opties)) {
    # 'intervals' werkt per veld; per veld (titel, beschrijving, tekst) dezelfde
    # eis, zodat contexttermen net zo breed zoeken als de andere termen
    kern <- if (woordvorm) {
      list(prefix = list(prefix = term))
    } else {
      list(match = list(query = term, ordered = TRUE, max_gaps = 0))
    }
    cultuur <- lapply(CULTUURWOORDEN, \(w) list(match = list(query = w)))
    per_veld <- lapply(VELDEN, function(veld) {
      list(intervals = setNames(list(list(all_of = list(
        ordered = FALSE, max_gaps = CONTEXT_AFSTAND,
        intervals = list(kern, list(any_of = list(intervals = cultuur)))
      ))), veld))
    })
    return(list(bool = list(should = per_veld, minimum_should_match = 1)))
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
  tot <- if (jaren[2] >= huidig_jaar()) "now+1d/d" else sprintf("%d-01-01", jaren[2] + 1)
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

# --- Blokkade door de API (HTTP 429) ---
# Na een 429 blokkeert de API ~10 minuten. Opnieuw proberen verlengt dat
# alleen, dus onthouden we de blokkade voor het hele proces (alle bezoekers)
# en melden we het direct, zonder verzoek.
API_BLOKKADE_MINUTEN <- 10
api_status <- new.env()

blokkade_tot <- function() {
  tot <- api_status$blokkade_tot
  if (!is.null(tot) && Sys.time() < tot) tot else NULL
}

zet_blokkade <- function(seconden = API_BLOKKADE_MINUTEN * 60) {
  api_status$blokkade_tot <- Sys.time() + seconden
}

blokkade_fout <- function() {
  minuten <- max(1, ceiling(as.numeric(difftime(blokkade_tot(), Sys.time(),
                                                units = "mins"))))
  rlang::abort(
    sprintf(paste("De API van OpenBesluitvorming krijgt even te veel",
                  "verzoeken (HTTP 429). Probeer het over ongeveer %d",
                  "minuten opnieuw."), minuten),
    class = "api_blokkade"
  )
}

post_json <- function(body, index = "ori_*", pogingen = 2) {
  sleutel <- rlang::hash(list(index, body))
  bewaard <- ori_cache$get(sleutel)
  if (!cachem::is.key_missing(bewaard)) return(bewaard)
  if (!is.null(blokkade_tot())) blokkade_fout()

  for (poging in seq_len(pogingen)) {
    resp <- request(API_BASIS) |>
      req_url_path_append(index, "_search") |>
      req_body_json(body) |>
      req_timeout(60) |>
      req_user_agent("cultuur-dashboard-mvp (R/httr2)") |>
      # Alleen korte serverstoringen opnieuw proberen, nooit een 429
      req_retry(max_tries = 3, max_seconds = 30, backoff = \(i) 2^i,
                is_transient = \(r) resp_status(r) %in% c(502, 503, 504)) |>
      req_error(is_error = function(r) FALSE) |>
      req_perform()

    if (resp_status(resp) == 429) {
      wacht <- suppressWarnings(as.numeric(resp_header(resp, "Retry-After")))
      zet_blokkade(if (isTRUE(wacht > 0)) wacht else API_BLOKKADE_MINUTEN * 60)
      blokkade_fout()
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


# --- Dekking en onzekerheid ---------------------------------------------------

# Per gemeente: vanaf welk jaar er een archief is, en over hoeveel jaren er
# in de gekozen periode echt documenten zijn. Een jaar telt als het minstens
# MIN_DOCS_DEKKING documenten heeft; het lopende jaar telt naar rato mee.
# Zo worden gemeenten die later instromen niet benadeeld.
dekking_per_gemeente <- function(trends, vandaag = Sys.Date()) {
  jaar_nu <- as.integer(format(vandaag, "%Y"))
  fractie_nu <- as.numeric(format(vandaag, "%j")) / 365
  trends |>
    filter(term == "__alle__", archief >= MIN_DOCS_DEKKING) |>
    group_by(key) |>
    summarise(eerste_jaar = min(jaar),
              jaren_dekking = sum(ifelse(jaar == jaar_nu, fractie_nu, 1)),
              .groups = "drop")
}

# Lengte van een periode in jaren, met het lopende jaar naar rato (zelfde
# rekenwijze als jaren_dekking)
periode_jaren <- function(jaren, vandaag = Sys.Date()) {
  jaar_nu <- as.integer(format(vandaag, "%Y"))
  j <- seq(jaren[1], min(jaren[2], jaar_nu))
  sum(ifelse(j == jaar_nu, as.numeric(format(vandaag, "%j")) / 365, 1))
}

# 95%-betrouwbaarheidsinterval voor een telling (Poisson, exact), omgerekend
# naar dezelfde schaal als de maatstaf. Bij kleine aantallen is dat breed.
poisson_interval <- function(n, noemer, schaal) {
  laag <- ifelse(n == 0, 0, stats::qchisq(0.025, 2 * n) / 2)
  hoog <- stats::qchisq(0.975, 2 * (n + 1)) / 2
  list(laag = schaal * laag / noemer, hoog = schaal * hoog / noemer)
}

# Maatstaven per gemeente: per 1.000 raadsdocumenten en per 100.000
# inwoners per jaar (over de jaren met dekking), met bandbreedte.
bereken_maatstaven <- function(per_gemeente) {
  per_gemeente |>
    mutate(
      genoeg_archief = !is.na(jaren_dekking) & jaren_dekking >= 1 &
        archief >= MIN_DOCS_PER_JAAR * jaren_dekking,
      per_1000 = ifelse(genoeg_archief, 1000 * totaal / archief, NA_real_),
      per_1000_laag = ifelse(genoeg_archief,
                             poisson_interval(totaal, archief, 1000)$laag, NA_real_),
      per_1000_hoog = ifelse(genoeg_archief,
                             poisson_interval(totaal, archief, 1000)$hoog, NA_real_),
      genoeg_inwoners = genoeg_archief & !is.na(inwoners) & inwoners >= MIN_INWONERS,
      per_100k = ifelse(genoeg_inwoners,
                        1e5 * totaal / inwoners / jaren_dekking, NA_real_),
      per_100k_laag = ifelse(genoeg_inwoners,
                             poisson_interval(totaal, inwoners * jaren_dekking, 1e5)$laag,
                             NA_real_),
      per_100k_hoog = ifelse(genoeg_inwoners,
                             poisson_interval(totaal, inwoners * jaren_dekking, 1e5)$hoog,
                             NA_real_),
      # Te weinig treffers voor een betekenisvolle plek in de ranking
      weinig_treffers = totaal < MIN_TREFFERS_RANG
    ) |>
    select(-genoeg_archief, -genoeg_inwoners)
}

# Eén request: per gemeente archiefgrootte, treffers per term en dezelfde
# cijfers per jaar, plus de 100 nieuwste treffers (post_filter raakt alleen de
# hits, niet de aggregaties). De landelijke trend is de som van de gemeenten:
# zo worden identieke stukken van verschillende gemeenten niet samengevoegd.
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
                       aggs = c(list(t = treffer_filters(termen, opties),
                                     jaren = jaren_agg(termen, opties)),
                                telling_aggs(opties$dedup)))
    )
  )
  json <- post_json(body)

  g_buckets <- json$aggregations$gemeenten$buckets
  if (is.null(g_buckets) || is.null(json$hits)) {
    stop("Onverwachte JSON-structuur (geen 'aggregations' of 'hits').")
  }
  if (length(g_buckets) == 0) stop("Geen documenten gevonden in deze periode.")

  per_index <- lapply(g_buckets, function(b) {
    n <- bucket_tellingen(b, termen, opties$dedup)
    ruw <- index_naar_ruw(b$key)
    key <- ruw_naar_key(ruw)
    list(
      totaal = tibble(key = key, ruw = ruw, archief = tel(b, opties$dedup),
                      totaal = n[["__alle__"]], !!!as.list(n[termen])),
      trend = parse_jaren(b$jaren$buckets, termen, opties$dedup) |>
        mutate(key = key)
    )
  })

  # Stadsdelen en fusiegemeenten optellen bij de huidige gemeente
  trends <- bind_rows(lapply(per_index, `[[`, "trend")) |>
    group_by(key, jaar, term) |>
    summarise(n = sum(n), archief = sum(archief), .groups = "drop")
  per_gemeente <- bind_rows(lapply(per_index, `[[`, "totaal")) |>
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

  per_gemeente <- per_gemeente |>
    left_join(dekking_per_gemeente(trends), by = "key") |>
    left_join(inwoners, by = "key") |>
    mutate(gemeente = coalesce(cbs_naam, nette_naam(key))) |>
    bereken_maatstaven() |>
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
    trends = trends,
    jaren_nl = trends |>
      group_by(jaar, term) |>
      summarise(n = sum(n), archief = sum(archief), .groups = "drop"),
    docs = docs,
    totaal_docs = sum(per_gemeente$totaal),
    termen = termen,
    jaren = jaren,
    opties = opties,
    inwoners_ok = nrow(inwoners) > 0,
    berekend_op = Sys.time(),
    schema = SCHEMA_VERSIE
  )
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
    # Gewone tekst met onze eigen markeringstekens; de app escapet alles zelf
    # en maakt daarna pas <mark> van de markeringen (zie markeer_html()). Zo
    # hangt de veiligheid niet af van hoe de bron-API escapet.
    highlight = list(
      highlight_query = markeer,
      pre_tags = list(MARK_BEGIN), post_tags = list(MARK_EIND),
      fields = list(text = list(fragment_size = 220, number_of_fragments = 2),
                    name = list(number_of_fragments = 0))
    )
  )
  json <- post_json(body, index = index_patroon(ruwe_namen))
  if (is.null(json$hits$hits)) stop("Onverwachte JSON-structuur voor fragmenten.")

  bind_rows(lapply(json$hits$hits, function(h) {
    s <- h[["_source"]]
    frag <- unlist(h$highlight$text %||% list())
    titel <- as.character(s$name %||% "(zonder titel)")
    tibble(
      titel = titel,
      datum = substr(as.character(s$last_discussed_at %||% ""), 1, 10),
      link = as.character(s$original_url %||% s$url %||% ""),
      # Stukken waarin vaak namen van burgers staan: geen fragment tonen
      privacy = grepl(PRIVACY_TITELS, tolower(titel)),
      fragmenten = paste(gsub("\\s+", " ", frag), collapse = " … ")
    )
  }))
}
