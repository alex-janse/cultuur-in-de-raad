# =============================================================================
# Kunst & Cultuur in de gemeenteraad — MVP dashboard
# Live data uit Open Raadsinformatie / OpenBesluitvorming.nl + CBS
#
# Starten:  shiny::runApp("pad/naar/deze/map")
# =============================================================================

library(shiny)
library(httr2)
library(jsonlite)
library(dplyr)
library(leaflet)
library(ggplot2)
library(cbsodataR)

# --- Configuratie ------------------------------------------------------------

# Publieke Elasticsearch-endpoint achter OpenBesluitvorming.nl.
# Elke gemeente heeft een eigen index ("ori_<gemeente>_<timestamp>"); die index
# is daarmee de 'source' waarop we aggregeren.
API_BASIS <- "https://api.openraadsinformatie.nl/v1/elastic"
MAX_DOCS <- 100          # aantal voorbeelddocumenten (limit=100)
MIN_DOCS_PER_JAAR <- 400 # kleinere archieven geven onbetrouwbare relatieve cijfers
MIN_INWONERS <- 20000    # zeer kleine gemeenten domineren anders 'per inwoner'
EERSTE_JAAR <- 2010
HUIDIG_JAAR <- as.integer(format(Sys.Date(), "%Y"))

CBS_TABEL_BEVOLKING <- "70072ned"  # Regionale kerncijfers Nederland

TERMEN <- list(
  "Kernthema's" = c("amateurkunst", "cultuurbeoefening", "cultuureducatie",
                    "kunstenplan", "bibliotheek"),
  "Aanverwante termen" = c("talentontwikkeling", "cultuurparticipatie",
                           "muziekonderwijs", "muziekschool", "podiumkunsten",
                           "cultuurbeleid", "erfgoed")
)
STANDAARD_TERMEN <- c("amateurkunst", "cultuurbeoefening", "talentontwikkeling")

MAATSTAVEN <- c("Per 1.000 raadsdocumenten" = "relatief",
                "Per 100.000 inwoners (per jaar)" = "inwoners",
                "Absoluut (aantal documenten)" = "absoluut")

# Hardcoded coördinaten; 'key' = genormaliseerde gemeentenaam (zie naar_key()).
GEMEENTEN <- tribble(
  ~key,             ~gemeente,          ~lat,    ~lon,
  "amsterdam",      "Amsterdam",        52.3676, 4.9041,
  "rotterdam",      "Rotterdam",        51.9244, 4.4777,
  "den_haag",       "Den Haag",         52.0705, 4.3007,
  "utrecht",        "Utrecht",          52.0907, 5.1214,
  "eindhoven",      "Eindhoven",        51.4416, 5.4697,
  "groningen",      "Groningen",        53.2194, 6.5665,
  "tilburg",        "Tilburg",          51.5555, 5.0913,
  "almere",         "Almere",           52.3508, 5.2647,
  "breda",          "Breda",            51.5719, 4.7683,
  "nijmegen",       "Nijmegen",         51.8126, 5.8372,
  "apeldoorn",      "Apeldoorn",        52.2112, 5.9699,
  "arnhem",         "Arnhem",           51.9851, 5.8987,
  "haarlem",        "Haarlem",          52.3874, 4.6462,
  "haarlemmermeer", "Haarlemmermeer",   52.3030, 4.6890,
  "amersfoort",     "Amersfoort",       52.1561, 5.3878,
  "zaanstad",       "Zaanstad",         52.4570, 4.7510,
  "enschede",       "Enschede",         52.2215, 6.8937,
  "den_bosch",      "'s-Hertogenbosch", 51.6978, 5.3037,
  "zwolle",         "Zwolle",           52.5168, 6.0830,
  "zoetermeer",     "Zoetermeer",       52.0575, 4.4931,
  "leiden",         "Leiden",           52.1601, 4.4970,
  "maastricht",     "Maastricht",       50.8514, 5.6910,
  "dordrecht",      "Dordrecht",        51.8133, 4.6901,
  "ede",            "Ede",              52.0402, 5.6649,
  "alkmaar",        "Alkmaar",          52.6324, 4.7534,
  "emmen",          "Emmen",            52.7792, 6.9069,
  "westland",       "Westland",         51.9990, 4.2090,
  "delft",          "Delft",            52.0116, 4.3571,
  "venlo",          "Venlo",            51.3704, 6.1724,
  "deventer",       "Deventer",         52.2661, 6.1552,
  "leeuwarden",     "Leeuwarden",       53.2012, 5.7999,
  "lelystad",       "Lelystad",         52.5185, 5.4714,
  "helmond",        "Helmond",          51.4793, 5.6570,
  "hilversum",      "Hilversum",        52.2292, 5.1669,
  "heerlen",        "Heerlen",          50.8882, 5.9795,
  "amstelveen",     "Amstelveen",       52.3114, 4.8701,
  "gouda",          "Gouda",            52.0115, 4.7105,
  "assen",          "Assen",            52.9925, 6.5649,
  "purmerend",      "Purmerend",        52.5050, 4.9597,
  "sittard_geleen", "Sittard-Geleen",   51.0000, 5.8686,
  "oss",            "Oss",              51.7650, 5.5180,
  "roosendaal",     "Roosendaal",       51.5308, 4.4653,
  "noordoostpolder","Noordoostpolder",  52.7100, 5.7500,
  "hoogeveen",      "Hoogeveen",        52.7225, 6.4764
)

# CBS-namen die niet automatisch op de ORI-indexnaam passen
CBS_NAAM_NAAR_KEY <- c(
  "Bergen (NH.)" = "bergen_nh",
  "Bergen (L.)" = "bergen",
  "'s-Gravenhage (gemeente)" = "den_haag",
  "'s-Hertogenbosch" = "den_bosch",
  "Capelle aan den IJssel" = "capelle_ad_ijssel",
  "Krimpen aan den IJssel" = "krimpen_ad_ijssel",
  "Bodegraven-Reeuwijk" = "bodegravenreeuwijk",
  "Hof van Twente" = "hofvantwente",
  "Kaag en Braassem" = "kaag_en_brasssem",
  "Nuenen, Gerwen en Nederwetten" = "nuenen"
)

# --- Hulpfuncties: namen -----------------------------------------------------

# "Súdwest-Fryslân" -> "sudwest_fryslan", "Utrecht (gemeente)" -> "utrecht"
naar_key <- function(x) {
  x <- tolower(sub(" [(][^)]*[)]$", "", x))
  x <- iconv(x, "UTF-8", "ASCII//TRANSLIT")
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

# "ori_midden-delfland_20250525191105" -> "midden-delfland"
index_naar_ruw <- function(index) sub("_[0-9]{8,}$", "", sub("^ori_", "", index))

# Ruwe indexnaam -> key; Amsterdamse stadsdelen worden bij Amsterdam opgeteld
ruw_naar_key <- function(ruw) sub("^amsterdam_.*$", "amsterdam", naar_key(ruw))

# Indexpatroon voor één gemeente ("_2*" voorkomt dat bergen ook bergen_nh pakt)
index_patroon <- function(ruwe_namen) {
  pat <- ifelse(grepl("^amsterdam", ruwe_namen), "ori_amsterdam*",
                paste0("ori_", ruwe_namen, "_2*"))
  paste(unique(pat), collapse = ",")
}

nette_naam <- function(key) {
  x <- gsub("_", " ", key)
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

fmt <- function(x, digits = 1) {
  ifelse(is.na(x), "–", formatC(x, format = "f", digits = digits,
                                big.mark = ".", decimal.mark = ","))
}

# --- CBS: inwoners per gemeente ----------------------------------------------

# Verandert hooguit jaarlijks: één keer per R-sessie ophalen.
cbs_cache <- new.env()
haal_inwoners <- function() {
  if (!is.null(cbs_cache$inwoners)) return(cbs_cache$inwoners)
  meta <- cbs_get_meta(CBS_TABEL_BEVOLKING)
  perioden <- tail(grep("JJ00$", meta$Perioden$Key, value = TRUE), 2)
  d <- cbs_get_data(CBS_TABEL_BEVOLKING, Perioden = perioden,
                    RegioS = has_substring("GM"),
                    select = c("RegioS", "Perioden", "TotaleBevolking_1"))
  if (nrow(d) == 0) stop("CBS gaf geen bevolkingsdata terug.")

  inw <- d |>
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
      cbs_naam = sub(" [(]gemeente[)]$", "", cbs_naam),
      inwoners = as.numeric(TotaleBevolking_1),
      inwoners_jaar = substr(Perioden, 1, 4)
    ) |>
    select(key, gemeentecode, cbs_naam, inwoners, inwoners_jaar)

  cbs_cache$inwoners <- inw
  inw
}

# --- CBS: gemeentelijke lasten voor cultuur (Iv3) -----------------------------

# "Gemeenten <jaar> onbewerkte Iv3-data" staan niet in de StatLine-catalogus
# maar op dataderden.cbs.nl. Bedragen in 1.000 euro ("1e plaatsing").
IV3_TABELLEN <- c(`2023` = "45063NED", `2024` = "45067NED",
                  `2025` = "45071NED", `2026` = "45078NED")
IV3_JAAR <- 2024                                  # meest recente jaarrekening
IV3_TAAKVELDEN <- c("5.3", "5.4", "5.5", "5.6")   # cultuur, musea, erfgoed, media/bibliotheek

haal_cultuurlasten <- function(jaar = IV3_JAAR) {
  if (!is.null(cbs_cache$lasten)) return(cbs_cache$lasten)

  tabel <- IV3_TABELLEN[[as.character(jaar)]]
  basis <- sprintf("https://dataderden.cbs.nl/ODataFeed/odata/%s", tabel)
  # Sleutels zijn met spaties aangevuld tot 6 tekens: '5.3   '
  filter <- sprintf(
    "Verslagsoort eq '%sX005' and startswith(Categorie,'L') and (%s)", jaar,
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
    req <- request(volgende) |> req_headers(Accept = "application/json")
  }

  inw_jaar <- min(jaar, HUIDIG_JAAR - 1)
  inwoners <- cbs_get_data(
    "03759ned", Perioden = paste0(inw_jaar, "JJ00"), Geslacht = "T001038",
    Leeftijd = "10000", BurgerlijkeStaat = "T001019",
    RegioS = has_substring("GM")
  ) |>
    transmute(gemeentecode = trimws(RegioS),
              inwoners_iv3 = as.numeric(BevolkingOp1Januari_1))

  lasten <- bind_rows(rijen) |>
    mutate(gemeentecode = trimws(Gemeenten)) |>
    group_by(gemeentecode) |>
    summarise(lasten_keur = sum(k_1ePlaatsing_1, na.rm = TRUE),
              .groups = "drop") |>
    left_join(inwoners, by = "gemeentecode") |>
    transmute(gemeentecode,
              # 0 betekent: (nog) niet aangeleverd
              cultuur_per_inw = ifelse(lasten_keur > 0,
                                       lasten_keur * 1000 / inwoners_iv3,
                                       NA_real_))

  cbs_cache$lasten <- lasten
  lasten
}

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

# --- UI ----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Kunst & cultuur in Nederlandse gemeenteraden"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectizeInput(
        "termen", "Zoektermen",
        choices = TERMEN,
        selected = STANDAARD_TERMEN,
        multiple = TRUE,
        options = list(
          create = TRUE, persist = TRUE,
          placeholder = "Kies of typ een term…",
          plugins = list("remove_button")
        )
      ),
      helpText("Typ zelf een term of woordgroep (bijv. 'kunst en cultuur')",
               "en druk op Enter om hem toe te voegen. Er wordt gezocht",
               "op de exacte woordgroep."),
      sliderInput("jaren", "Periode (vergaderdatum)",
                  min = EERSTE_JAAR, max = HUIDIG_JAAR,
                  value = c(2020, HUIDIG_JAAR), step = 1, sep = ""),
      actionButton("ophalen", "Haal Live Data Op",
                   class = "btn-primary", width = "100%"),
      hr(),
      radioButtons("maatstaf", "Maatstaf", choices = MAATSTAVEN,
                   selected = "relatief"),
      helpText(sprintf(paste(
        "'Per 1.000 raadsdocumenten' corrigeert voor de omvang van het",
        "archief; gemeenten met minder dan %d documenten per jaar tellen",
        "dan niet mee. Inwoners: CBS."), MIN_DOCS_PER_JAAR)),
      hr(),
      uiOutput("status"),
      hr(),
      helpText("Bron: OpenBesluitvorming.nl / Open Raadsinformatie en CBS.",
               "Telling = aantal raadsdocumenten waarin een term voorkomt.")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Kaart & ranking", value = "kaart",
          br(),
          leafletOutput("kaart", height = 480),
          helpText("Klik op een cirkel om de trend van die gemeente te zien."),
          tableOutput("tabel")
        ),
        tabPanel(
          "Trend", value = "trend",
          br(),
          selectInput("trend_gemeente", "Gemeente",
                      choices = c("Heel Nederland" = "NL"), width = "300px"),
          plotOutput("trend", height = 420),
          helpText("Bij 'per 1.000 raadsdocumenten' is de noemer het aantal",
                   "documenten in dat jaar, zodat groei van het archief niet",
                   "als groei van aandacht telt. Het lopende jaar is nog niet",
                   "compleet.")
        ),
        tabPanel(
          "Aandacht vs. budget", value = "budget",
          br(),
          plotOutput("budget_plot", height = 520,
                     hover = hoverOpts("budget_hover", delay = 100)),
          uiOutput("budget_hover_info"),
          helpText(sprintf(paste(
            "Budget: gemeentelijke lasten voor cultuur per inwoner uit de",
            "jaarrekening %d (CBS Iv3, taakvelden 5.3 cultuur, 5.4 musea,",
            "5.5 erfgoed en 5.6 media/bibliotheek; inclusief kapitaallasten",
            "en verrekeningen). Stippellijnen = mediaan. Alleen gemeenten",
            "met minstens %s inwoners."), IV3_JAAR,
            fmt(MIN_INWONERS, 0))),
          tableOutput("budget_tabel")
        ),
        tabPanel(
          sprintf("Documenten (nieuwste %d)", MAX_DOCS), value = "docs",
          br(),
          tableOutput("documenten")
        )
      )
    )
  )
)

# --- Server ------------------------------------------------------------------

server <- function(input, output, session) {

  resultaat <- reactiveVal(NULL)
  foutmelding <- reactiveVal(NULL)

  observeEvent(input$ophalen, {
    termen <- unique(tolower(trimws(input$termen)))
    termen <- termen[nchar(termen) >= 2]
    if (length(termen) == 0) {
      showNotification("Kies minstens één zoekterm.", type = "warning")
      return()
    }

    id <- showNotification("Live data ophalen…", duration = NULL,
                           closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)

    res <- withCallingHandlers(
      tryCatch(
        haal_data_op(termen, input$jaren),
        httr2_failure = function(e) {
          foutmelding(paste("Geen verbinding met de API:", conditionMessage(e)))
          NULL
        },
        error = function(e) {
          foutmelding(paste("Fout bij ophalen/verwerken:", conditionMessage(e)))
          NULL
        }
      ),
      warning = function(w) {
        showNotification(conditionMessage(w), type = "warning", duration = 8)
        invokeRestart("muffleWarning")
      }
    )

    if (is.null(res)) {
      showNotification(foutmelding(), type = "error", duration = 8)
      return()
    }
    foutmelding(NULL)
    resultaat(res)

    met_treffers <- res$per_gemeente |> filter(totaal > 0) |> arrange(gemeente)
    updateSelectInput(session, "trend_gemeente", choices = c(
      "Heel Nederland" = "NL",
      setNames(met_treffers$key, met_treffers$gemeente)
    ), selected = isolate(input$trend_gemeente))
    if (nrow(met_treffers) == 0) {
      showNotification("Geen resultaten gevonden voor deze termen.",
                       type = "warning")
    }
  })

  output$status <- renderUI({
    if (!is.null(foutmelding())) {
      return(tags$div(class = "text-danger", foutmelding()))
    }
    res <- resultaat()
    if (is.null(res)) return(helpText("Nog geen data opgehaald."))
    met_treffers <- sum(res$per_gemeente$totaal > 0)
    tags$div(
      tags$b(fmt(res$totaal_docs, 0)), " documenten in ",
      tags$b(met_treffers), " gemeenten",
      tags$br(), tags$small(sprintf("%d–%d · %s", res$jaren[1], res$jaren[2],
                                    paste(res$termen, collapse = ", "))),
      if (!res$inwoners_ok) {
        tags$div(class = "text-warning", "CBS-inwonersdata niet beschikbaar.")
      }
    )
  })

  # Resultaten met een kolom 'waarde' volgens de gekozen maatstaf, gesorteerd
  gerangschikt <- reactive({
    res <- resultaat()
    req(res)
    kolom <- switch(input$maatstaf, relatief = "per_1000",
                    inwoners = "per_100k", absoluut = "totaal")
    res$per_gemeente |>
      filter(totaal > 0) |>
      mutate(waarde = .data[[kolom]]) |>
      filter(!is.na(waarde)) |>
      arrange(desc(waarde))
  })

  # --- Kaart ---

  output$kaart <- renderLeaflet({
    leaflet() |>
      # PDOK-achtergrondkaart (Kadaster): gratis, geen API-key nodig
      addTiles(
        urlTemplate = "https://service.pdok.nl/brt/achtergrondkaart/wmts/v2_0/grijs/EPSG:3857/{z}/{x}/{y}.png",
        attribution = "Kaartgegevens &copy; <a href='https://www.kadaster.nl'>Kadaster</a>",
        options = tileOptions(minZoom = 6, maxZoom = 19)
      ) |>
      setView(lng = 5.3, lat = 52.2, zoom = 7)
  })

  observe({
    punten <- gerangschikt() |> filter(!is.na(lat), waarde > 0)

    proxy <- leafletProxy("kaart") |> clearMarkers()
    if (nrow(punten) == 0) return()

    eenheid <- switch(input$maatstaf,
                      relatief = "per 1.000 docs",
                      inwoners = "per 100.000 inw./jaar",
                      absoluut = "documenten")
    maxn <- max(punten$waarde)
    punten <- punten |>
      mutate(
        straal = 6 + 30 * sqrt(waarde / maxn),
        label = sprintf("%s: %s %s", gemeente,
                        fmt(waarde, if (input$maatstaf == "absoluut") 0 else 1),
                        eenheid),
        popup = sprintf(paste(
          "<b>%s</b><br>%s documenten<br>%s per 1.000 raadsdocumenten",
          "<br>%s per 100.000 inwoners per jaar"),
          gemeente, fmt(totaal, 0), fmt(per_1000), fmt(per_100k))
      )

    proxy |>
      addCircleMarkers(
        data = punten, lng = ~lon, lat = ~lat, radius = ~straal,
        layerId = ~key,
        stroke = TRUE, weight = 1, color = "#7a1f5c",
        fillColor = "#c2378f", fillOpacity = 0.55,
        label = ~label, popup = ~popup
      )
  })

  observeEvent(input$kaart_marker_click, {
    updateSelectInput(session, "trend_gemeente",
                      selected = input$kaart_marker_click$id)
    updateTabsetPanel(session, "tabs", selected = "trend")
  })

  # --- Tabel ---

  output$tabel <- renderTable({
    res <- resultaat()
    df <- gerangschikt()
    shiny::validate(need(nrow(df) > 0, "Geen resultaten."))
    df |>
      head(25) |>
      mutate(across(all_of(res$termen), \(x) fmt(x, 0))) |>
      transmute(
        `#` = row_number(),
        Gemeente = gemeente,
        `Per 1.000 docs` = fmt(per_1000),
        `Per 100k inw./jaar` = fmt(per_100k),
        Totaal = fmt(totaal, 0),
        across(all_of(res$termen)),
        `Archief (docs)` = fmt(archief, 0),
        Inwoners = fmt(inwoners, 0),
        `Op kaart` = ifelse(is.na(lat), "", "✓")
      )
  }, striped = TRUE, hover = TRUE, align = "l")

  # --- Trend ---

  trend_data <- reactive({
    res <- resultaat()
    req(res, input$trend_gemeente)
    if (input$trend_gemeente == "NL") {
      return(list(data = res$jaren_nl, naam = "heel Nederland"))
    }
    rij <- res$per_gemeente |> filter(key == input$trend_gemeente)
    req(nrow(rij) == 1)
    data <- tryCatch(
      haal_trend_gemeente(rij$ruw[[1]], res$termen, res$jaren),
      error = function(e) {
        showNotification(paste("Trend ophalen mislukt:", conditionMessage(e)),
                         type = "error", duration = 8)
        NULL
      }
    )
    req(data)
    list(data = data, naam = rij$gemeente)
  })

  output$trend <- renderPlot({
    td <- trend_data()
    res <- resultaat()
    relatief <- input$maatstaf != "absoluut"

    df <- td$data |>
      mutate(
        term = ifelse(term == "__alle__", "Alle gekozen termen", term),
        waarde = if (relatief) {
          # jaren met heel weinig documenten geven wilde uitschieters
          ifelse(archief >= 50, 1000 * n / archief, NA_real_)
        } else n
      )
    if (length(res$termen) == 1) df <- df |> filter(term != "Alle gekozen termen")
    shiny::validate(need(any(df$waarde > 0, na.rm = TRUE),
                         "Geen treffers in deze periode."))

    ggplot(df, aes(jaar, waarde, colour = term)) +
      geom_line(aes(linewidth = term == "Alle gekozen termen"), na.rm = TRUE) +
      geom_point(size = 2, na.rm = TRUE) +
      scale_linewidth_manual(values = c(`FALSE` = 0.8, `TRUE` = 1.6),
                             guide = "none") +
      scale_x_continuous(breaks = seq(res$jaren[1], res$jaren[2], by = 1)) +
      scale_y_continuous(labels = \(x) fmt(x, if (relatief) 1 else 0),
                         limits = c(0, NA)) +
      labs(
        title = sprintf("Trend in %s", td$naam),
        x = NULL, colour = NULL,
        y = if (relatief) "Documenten per 1.000 raadsdocumenten"
            else "Aantal documenten"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank())
  })

  # --- Aandacht vs. budget ---

  # Iv3-data pas ophalen als iemand het tabblad opent (duurt 5-10 s)
  lasten <- reactiveVal(NULL)
  observeEvent(input$tabs, {
    req(input$tabs == "budget", is.null(lasten()))
    id <- showNotification("Cultuurbudgetten ophalen bij CBS…",
                           duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    tryCatch(
      lasten(haal_cultuurlasten()),
      error = function(e) {
        showNotification(paste("CBS-budgetdata ophalen mislukt:",
                               conditionMessage(e)),
                         type = "error", duration = 10)
      }
    )
  })

  aandacht_kolom <- reactive({
    if (input$maatstaf == "inwoners") "per_100k" else "per_1000"
  })
  aandacht_label <- reactive({
    if (input$maatstaf == "inwoners") {
      "Aandacht: documenten per 100.000 inwoners per jaar"
    } else {
      "Aandacht: documenten per 1.000 raadsdocumenten"
    }
  })

  budget_df <- reactive({
    res <- resultaat()
    shiny::validate(need(res, "Haal eerst live data op."))
    shiny::validate(need(lasten(), "Budgetdata nog niet beschikbaar."))
    df <- res$per_gemeente |>
      filter(!is.na(inwoners), inwoners >= MIN_INWONERS) |>
      inner_join(lasten(), by = "gemeentecode") |>
      mutate(aandacht = .data[[aandacht_kolom()]]) |>
      filter(!is.na(aandacht), !is.na(cultuur_per_inw))
    shiny::validate(need(nrow(df) >= 5, "Te weinig gemeenten om te vergelijken."))

    # Verschil in percentielrang: positief = veel aandacht t.o.v. budget
    df |>
      mutate(
        rang_aandacht = percent_rank(aandacht),
        rang_budget = percent_rank(cultuur_per_inw),
        verschil = rang_aandacht - rang_budget,
        profiel = case_when(
          aandacht >= median(aandacht) & cultuur_per_inw < median(cultuur_per_inw) ~
            "Veel aandacht, weinig budget",
          aandacht < median(aandacht) & cultuur_per_inw >= median(cultuur_per_inw) ~
            "Veel budget, weinig aandacht",
          aandacht >= median(aandacht) ~ "Veel aandacht, veel budget",
          TRUE ~ "Weinig aandacht, weinig budget"
        )
      )
  })

  output$budget_plot <- renderPlot({
    df <- budget_df()
    rho <- suppressWarnings(cor(df$aandacht, df$cultuur_per_inw,
                                method = "spearman"))
    labels <- df |>
      filter(rank(-aandacht) <= 6 | rank(-cultuur_per_inw) <= 4 |
               rank(-abs(verschil)) <= 6)

    ggplot(df, aes(cultuur_per_inw, aandacht)) +
      geom_vline(xintercept = median(df$cultuur_per_inw), linetype = "dashed",
                 colour = "grey60") +
      geom_hline(yintercept = median(df$aandacht), linetype = "dashed",
                 colour = "grey60") +
      geom_point(aes(size = inwoners, colour = profiel), alpha = 0.7) +
      ggrepel::geom_text_repel(data = labels, aes(label = gemeente),
                               size = 3.8, max.overlaps = 30,
                               min.segment.length = 0) +
      scale_x_log10(labels = \(x) paste0("€", fmt(x, 0))) +
      scale_size_area(max_size = 12, guide = "none") +
      scale_colour_manual(values = c(
        "Veel aandacht, weinig budget" = "#c2378f",
        "Veel budget, weinig aandacht" = "#2b7bba",
        "Veel aandacht, veel budget"   = "#5a3e8c",
        "Weinig aandacht, weinig budget" = "grey55"
      )) +
      labs(
        title = "Praat de raad over cultuur in verhouding tot wat de gemeente eraan uitgeeft?",
        subtitle = sprintf(
          "%d gemeenten · %d–%d · Spearman-correlatie: %s",
          nrow(df), resultaat()$jaren[1], resultaat()$jaren[2],
          fmt(rho, 2)),
        x = sprintf("Lasten cultuur per inwoner, %d (logaritmische schaal)",
                    IV3_JAAR),
        y = aandacht_label(), colour = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank(),
            plot.title = element_text(size = 14, face = "bold"))
  })

  output$budget_hover_info <- renderUI({
    df <- budget_df()
    h <- input$budget_hover
    req(h)
    # nearPoints kent de log-schaal niet; zelf de dichtstbijzijnde zoeken
    afstand <- (log10(df$cultuur_per_inw) - h$x)^2 /
      diff(log10(range(df$cultuur_per_inw)))^2 +
      (df$aandacht - h$y)^2 / diff(range(df$aandacht))^2
    i <- which.min(afstand)
    req(afstand[i] < 0.001)
    r <- df[i, ]
    tags$div(
      style = "padding:4px 0;",
      tags$b(r$gemeente), sprintf(
        " — %s aandacht · €%s per inwoner aan cultuur · %s inwoners · %s",
        fmt(r$aandacht), fmt(r$cultuur_per_inw, 0), fmt(r$inwoners, 0),
        r$profiel)
    )
  })

  output$budget_tabel <- renderTable({
    budget_df() |>
      arrange(desc(abs(verschil))) |>
      head(15) |>
      transmute(
        Gemeente = gemeente,
        Profiel = profiel,
        Aandacht = fmt(aandacht),
        `Cultuur €/inw.` = fmt(cultuur_per_inw, 0),
        `Pct. aandacht` = fmt(100 * rang_aandacht, 0),
        `Pct. budget` = fmt(100 * rang_budget, 0)
      )
  }, striped = TRUE, hover = TRUE, align = "l",
  caption = "Grootste verschillen tussen aandacht en budget (percentielrang 0–100)",
  caption.placement = "top")

  # --- Documenten ---

  output$documenten <- renderTable({
    res <- resultaat()
    req(res)
    shiny::validate(need(nrow(res$docs) > 0, "Geen documenten."))
    res$docs |>
      mutate(titel = ifelse(
        nzchar(link),
        sprintf('<a href="%s" target="_blank">%s</a>',
                htmltools::htmlEscape(link, attribute = TRUE),
                htmltools::htmlEscape(titel)),
        htmltools::htmlEscape(titel))) |>
      select(Gemeente = gemeente, Datum = datum, Document = titel)
  }, striped = TRUE, sanitize.text.function = identity)
}

shinyApp(ui, server)
