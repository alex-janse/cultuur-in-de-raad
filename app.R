# =============================================================================
# Kunst & cultuur in de gemeenteraad — dashboard
# Live data uit Open Raadsinformatie / OpenBesluitvorming.nl + CBS + PDOK
#
# Starten:  shiny::runApp("pad/naar/deze/map")
# Hulpfuncties staan in R/ en worden door Shiny automatisch geladen.
# =============================================================================

# Huisstijl: kleuren van LKCA (zie KLEUR in R/config.R), lettertypen Barlow
# (koppen, lijkt op het DIN van LKCA) en Heebo (tekst) via Google Fonts
LETTERTYPEN <- paste0(
  "https://fonts.googleapis.com/css2?family=Barlow:wght@500;600;700",
  "&family=Heebo:wght@400;500;700&display=swap"
)

CSS <- "
body { font-family: 'Heebo', -apple-system, 'Segoe UI', Roboto, sans-serif;
       color: #272727; }
h1, h2, h3, h4, .control-label, .nav-tabs > li > a, .btn {
  font-family: 'Barlow', 'Heebo', sans-serif; }
h2 { font-weight: 700; color: #5C1A82; }
h3 { font-weight: 600; color: #5C1A82; margin-top: 24px; }
a { color: #006CB2; }
.well { background: #F0F0F0; border: none; box-shadow: none; }
.btn-primary, .btn-primary:focus { background: #5C1A82; border-color: #5C1A82; }
.btn-primary:hover, .btn-primary:active { background: #3E0F59; border-color: #3E0F59; }
.nav-tabs > li.active > a, .nav-tabs > li.active > a:hover,
.nav-tabs > li.active > a:focus { color: #5C1A82; font-weight: 600;
  border-top: 3px solid #5C1A82; }
input[type=checkbox], input[type=radio] { accent-color: #5C1A82; }
.irs--shiny .irs-bar, .irs--shiny .irs-from, .irs--shiny .irs-to,
.irs--shiny .irs-single { background: #5C1A82; border-color: #5C1A82; }
mark { background: #FFF3A3; padding: 0 2px; border-radius: 2px; }
.kerncijfers { display: flex; flex-wrap: wrap; gap: 12px; margin: 8px 0 12px; }
.kerncijfer { flex: 1 1 150px; background: #F0F0F0; border-radius: 6px;
              padding: 10px 14px; }
.kerncijfer .waarde { font-family: 'Barlow', sans-serif; font-size: 24px;
                      font-weight: 700; color: #5C1A82; }
.kerncijfer .uitleg { font-size: 12px; color: #555; }
.verhaal { background: #EFE6F5; border-left: 4px solid #5C1A82;
           padding: 10px 16px; margin: 0 0 16px; font-size: 15px; }
.verhaal p { margin: 4px 0; }
.fragment { border-left: 3px solid #5C1A82; padding: 4px 12px; margin: 10px 0; }
.fragment .meta { font-size: 12px; color: #555; }
.knoppen { margin: 8px 0; }
#budget_tabel, #documenten { overflow-x: auto; }
.testbalk { margin: 12px 0 0; padding: 8px 14px; background: #FFED00;
            border: none; color: #272727; }
.uitleg-tekst { max-width: 760px; font-size: 15px; line-height: 1.6; }
.ranking-kop { display: flex; flex-wrap: wrap; gap: 16px; align-items: flex-end; }
"

# --- Achtergrondproces voor live zoeken --------------------------------------
# Een live zoekvraag duurt 5-15 s. In een apart R-proces (mirai) blijft de
# app intussen voor alle bezoekers bruikbaar. Met options(cultuur.async =
# FALSE) draait alles in het hoofdproces (handig voor tests).
ASYNC <- getOption("cultuur.async", TRUE)
if (ASYNC) {
  # Lukt het starten niet (bv. een host die geen extra processen toestaat),
  # dan zoekt de app gewoon in het hoofdproces, zoals voorheen.
  ASYNC <- tryCatch({
    mirai::daemons(1)
    mirai::everywhere({
      setwd(app_map)
      suppressMessages(shiny::loadSupport(app_map, renv = globalenv()))
    }, app_map = normalizePath("."))
    onStop(function() mirai::daemons(0))
    TRUE
  }, error = function(e) {
    message("Achtergrondproces niet gestart, zoeken zonder: ", conditionMessage(e))
    FALSE
  })
}

# Live opgehaalde resultaten, gedeeld door alle bezoekers van dit proces
resultaat_cache <- cachem::cache_mem(max_age = 3600)

# --- UI ----------------------------------------------------------------------

ui <- function(request) {
  fluidPage(
    lang = "nl",
    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com",
                crossorigin = NA),
      tags$link(rel = "stylesheet", href = LETTERTYPEN),
      tags$style(HTML(CSS))
    ),
    useBusyIndicators(),
    if (TESTVERSIE) {
      div(class = "alert alert-warning testbalk", role = "status",
          tags$b("Testversie."),
          " De cijfers worden nog gevalideerd; gebruik ze nog niet in",
          " publicaties. Feedback? Laat het weten aan degene die je deze link",
          " stuurde.")
    },
    titlePanel("Kunst & cultuur in Nederlandse gemeenteraden"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectizeInput("ga_naar", "Ga naar gemeente", choices = NULL,
                       options = list(placeholder = "Typ een gemeentenaam…")),
        hr(),
        selectInput("thema", "Thema",
                    choices = c("— eigen keuze —" = "", names(THEMASETS))),
        selectizeInput(
          "termen", "Zoektermen",
          choices = TERMEN,
          selected = STANDAARD_TERMEN,
          multiple = TRUE,
          options = list(
            create = TRUE, persist = TRUE, maxItems = MAX_TERMEN,
            placeholder = "Kies of typ een term…",
            plugins = list("remove_button")
          )
        ),
        helpText("Typ zelf een term of woordgroep (bijv. 'kunst en cultuur')",
                 "en druk op Enter om hem toe te voegen."),
        sliderInput("jaren", "Periode (vergaderdatum)",
                    min = EERSTE_JAAR, max = huidig_jaar(),
                    value = standaard_periode(), step = 1, sep = ""),
        checkboxInput("opt_context", "Alleen in cultuurcontext",
                      value = STANDAARD_OPTIES$context),
        checkboxInput("opt_dedup", "Dubbele bijlagen samenvoegen",
                      value = STANDAARD_OPTIES$dedup),
        checkboxInput("opt_woordvormen", "Ook woordvormen (amateurkunst*)",
                      value = STANDAARD_OPTIES$woordvormen),
        helpText(sprintf(paste(
          "Cultuurcontext: een term telt alleen als binnen %d woorden een",
          "cultuurwoord staat (cultuur, kunst, muziek, theater, …). Geldt",
          "niet voor termen die zelf al over cultuur gaan."), CONTEXT_AFSTAND)),
        bslib::input_task_button("ophalen", "Zoeken",
                                 label_busy = "Bezig met zoeken…",
                                 class = "btn-primary", width = "100%"),
        hr(),
        radioButtons("maatstaf", "Maatstaf", choices = MAATSTAVEN,
                     selected = "relatief"),
        helpText(sprintf(paste(
          "'Per 1.000 raadsdocumenten' corrigeert voor de omvang van het",
          "archief (minimaal %d documenten per jaar). 'Per inwoner' telt",
          "alleen gemeenten vanaf %s inwoners (CBS)."),
          MIN_DOCS_PER_JAAR, fmt(MIN_INWONERS, 0))),
        hr(),
        uiOutput("status"),
        div(class = "knoppen",
            bookmarkButton("Link naar deze zoekopdracht",
                           title = "Maak een link die deze instellingen bewaart")),
        hr(),
        helpText("Bronnen: OpenBesluitvorming.nl / Open Raadsinformatie,",
                 "CBS (inwoners, Iv3-gemeentefinanciën) en PDOK/Kadaster.",
                 "Telling = aantal raadsdocumenten waarin een term voorkomt."),
        tags$details(
          tags$summary(tags$small("Privacy")),
          helpText(PRIVACY_TEKST)
        )
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          id = "tabs",
          tabPanel(
            "Kaart & ranking", value = "kaart",
            br(),
            leafletOutput("kaart", height = 520),
            helpText("Klik op een gemeente voor het gemeenteprofiel. Grijs:",
                     "geen archief of te weinig documenten voor deze maatstaf."),
            div(class = "ranking-kop",
                selectInput("klasse", "Vergelijk met",
                            choices = c("Alle gemeenten" = "",
                                        setNames(GROOTTEKLASSE_NAMEN,
                                                 paste("Gemeenten met",
                                                       GROOTTEKLASSE_NAMEN))),
                            width = "280px"),
                div(class = "knoppen form-group",
                    downloadButton("dl_ranking", "Ranking (CSV)"))),
            DT::DTOutput("tabel"),
            helpText(paste("Bandbreedte: 95%-interval; overlappen twee",
                           "bandbreedtes, dan is het verschil niet betekenisvol.",
                           "Gemeenten met minder dan", MIN_TREFFERS_RANG,
                           "treffers krijgen geen rang. Klik op een kolomkop",
                           "om te sorteren."))
          ),
          tabPanel(
            "Trend", value = "trend",
            br(),
            selectizeInput(
              "trend_gebieden", "Vergelijk (maximaal 4)",
              choices = c("Heel Nederland" = "NL"), selected = "NL",
              multiple = TRUE, width = "100%",
              options = list(maxItems = 4, plugins = list("remove_button"))
            ),
            plotOutput("trend", height = 520),
            div(class = "knoppen",
                downloadButton("dl_trend", "Grafiek (PNG)")),
            helpText("Elke term heeft een eigen schaal. Bij 'per 1.000",
                     "raadsdocumenten' is de noemer het aantal documenten in",
                     "dat jaar, zodat groei van het archief niet als groei",
                     "van aandacht telt. Het lopende jaar is nog niet compleet.")
          ),
          tabPanel(
            "Gemeenteprofiel", value = "profiel",
            br(),
            selectInput("profiel_gemeente", "Gemeente", choices = NULL),
            uiOutput("profiel_kerncijfers"),
            uiOutput("profiel_verhaal"),
            fluidRow(
              column(7, plotOutput("profiel_trend", height = 400)),
              column(5, plotOutput("profiel_budget", height = 400))
            ),
            h4("Waar gaat het over? De nieuwste vermeldingen"),
            uiOutput("profiel_fragmenten")
          ),
          tabPanel(
            "Aandacht vs. budget", value = "budget",
            br(),
            selectInput("budget_keuze", "Budget", choices = IV3_KEUZES),
            plotOutput("budget_plot", height = 560,
                       hover = hoverOpts("budget_hover", delay = 100)),
            uiOutput("budget_hover_info"),
            div(class = "knoppen",
                downloadButton("dl_budget_png", "Grafiek (PNG)"),
                downloadButton("dl_budget_csv", "Gegevens (CSV)")),
            helpText(sprintf(paste(
              "Budget: gemeentelijke lasten voor cultuur per inwoner (CBS Iv3,",
              "taakvelden 5.3 cultuur, 5.4 musea, 5.5 erfgoed en 5.6",
              "media/bibliotheek; inclusief kapitaallasten en verrekeningen).",
              "Stippellijnen = mediaan. Alleen gemeenten met minstens %s",
              "inwoners."), fmt(MIN_INWONERS, 0))),
            tableOutput("budget_tabel")
          ),
          tabPanel(
            sprintf("Documenten (nieuwste %d)", MAX_DOCS), value = "docs",
            br(),
            div(class = "knoppen",
                downloadButton("dl_docs", "Documenten (CSV)")),
            tableOutput("documenten")
          ),
          tabPanel("Uitleg", value = "uitleg", br(), uitleg_ui())
        )
      )
    )
  )
}

# --- Server ------------------------------------------------------------------

server <- function(input, output, session) {

  resultaat <- reactiveVal(NULL)
  foutmelding <- reactiveVal(NULL)

  # --- Ophalen ---

  observeEvent(input$thema, {
    req(nzchar(input$thema))
    updateSelectizeInput(session, "termen", selected = THEMASETS[[input$thema]])
  })

  zoekopties <- reactive(list(
    context = isTRUE(input$opt_context),
    woordvormen = isTRUE(input$opt_woordvormen),
    dedup = isTRUE(input$opt_dedup)
  ))

  meld_fout <- function(tekst) {
    foutmelding(tekst)
    showNotification(tekst, type = "error", duration = 10)
  }

  # Een (nieuw) resultaat tonen en de keuzelijsten bijwerken
  verwerk_resultaat <- function(res, trend_selectie = NULL,
                                profiel_selectie = NULL) {
    for (m in res$meldingen) {
      showNotification(m, type = "warning", duration = 10)
    }
    foutmelding(NULL)
    resultaat(res)

    # Alle gemeenten met een archief, ook die zonder treffers (een echte nul)
    met_archief <- res$per_gemeente |> arrange(gemeente)
    met_treffers <- met_archief |> filter(totaal > 0)
    keuzes <- setNames(met_archief$key, met_archief$gemeente)
    trend_selectie <- trend_selectie %||% isolate(input$trend_gebieden)
    updateSelectizeInput(
      session, "trend_gebieden",
      choices = c("Heel Nederland" = "NL", keuzes),
      selected = intersect(trend_selectie %||% "NL", c("NL", keuzes))
    )
    profiel_selectie <- profiel_selectie %||% isolate(input$profiel_gemeente)
    updateSelectInput(
      session, "profiel_gemeente", choices = keuzes,
      selected = if (isTRUE(profiel_selectie %in% keuzes)) profiel_selectie
                 else met_treffers$key[which.max(met_treffers$totaal)]
    )
    updateSelectizeInput(session, "ga_naar", choices = c("", keuzes),
                         selected = "")
    if (nrow(met_treffers) == 0) {
      showNotification("Geen resultaten gevonden voor deze termen.",
                       type = "warning")
    }
  }

  # Selecties uit een gedeelde link, toe te passen zodra het live resultaat er is
  wachtende_selectie <- reactiveVal(list())

  # Live zoeken in een apart proces, zodat de app voor anderen (en voor deze
  # bezoeker) bruikbaar blijft terwijl de API antwoordt.
  zoek_taak <- ExtendedTask$new(function(termen, jaren, opties) {
    mirai::mirai({
      meldingen <- character()
      res <- withCallingHandlers(
        haal_data_op(termen, jaren, opties),
        warning = function(w) {
          meldingen <<- c(meldingen, conditionMessage(w))
          invokeRestart("muffleWarning")
        })
      res$bron <- "live"
      res$meldingen <- meldingen
      res
    }, termen = termen, jaren = jaren, opties = opties)
  })
  if (ASYNC) bslib::bind_task_button(zoek_taak, "ophalen")

  observeEvent(zoek_taak$status(), {
    status <- zoek_taak$status()
    if (status == "success") {
      res <- zoek_taak$result()
      resultaat_cache$set(zoek_sleutel(res$termen, res$jaren, res$opties), res)
      sel <- wachtende_selectie()
      verwerk_resultaat(res, sel$trend, sel$profiel)
      wachtende_selectie(list())
    } else if (status == "error") {
      tekst <- tryCatch({ zoek_taak$result(); "" },
                        error = function(e) conditionMessage(e))
      # De blokkade van het achtergrondproces ook hier onthouden
      if (grepl("429", tekst)) zet_blokkade()
      meld_fout(if (grepl("429", tekst)) tekst
                else paste("Fout bij ophalen/verwerken:", tekst))
    }
  })

  # Ook gebruikt bij het openen van een gedeelde link (zie onRestored)
  doe_ophalen <- function(termen, jaren, opties,
                          trend_selectie = NULL, profiel_selectie = NULL) {
    termen <- schoon_termen(termen)
    if (length(termen) == 0) {
      showNotification("Kies minstens één zoekterm.", type = "warning")
      return(invisible(FALSE))
    }
    jaren <- as.integer(jaren)

    # 1. Voorberekend of al eerder in dit proces opgehaald: direct tonen
    sleutel <- zoek_sleutel(termen, jaren, opties)
    res <- zoek_voorberekend(termen, jaren, opties)
    if (is.null(res)) {
      bewaard <- resultaat_cache$get(sleutel)
      if (!cachem::is.key_missing(bewaard)) res <- bewaard
    }
    if (!is.null(res)) {
      verwerk_resultaat(res, trend_selectie, profiel_selectie)
      return(invisible(TRUE))
    }

    # 2. Tijdens een blokkade van de API niet opnieuw proberen
    if (!is.null(blokkade_tot())) {
      meld_fout(tryCatch(blokkade_fout(), error = conditionMessage))
      return(invisible(FALSE))
    }

    # 3. Live ophalen (op de achtergrond)
    if (ASYNC) {
      wachtende_selectie(list(trend = trend_selectie, profiel = profiel_selectie))
      zoek_taak$invoke(termen, jaren, opties)
      return(invisible(TRUE))
    }
    id <- showNotification("Live data ophalen…", duration = NULL,
                           closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    meldingen <- character()
    res <- withCallingHandlers(
      tryCatch(haal_data_op(termen, jaren, opties), error = function(e) e),
      warning = function(w) {
        meldingen <<- c(meldingen, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    if (inherits(res, "error")) {
      meld_fout(if (inherits(res, "api_blokkade")) conditionMessage(res)
                else paste("Fout bij ophalen/verwerken:", conditionMessage(res)))
      return(invisible(FALSE))
    }
    res$bron <- "live"
    res$meldingen <- meldingen
    resultaat_cache$set(sleutel, res)
    verwerk_resultaat(res, trend_selectie, profiel_selectie)
    invisible(TRUE)
  }

  observeEvent(input$ophalen, {
    doe_ophalen(input$termen, input$jaren, zoekopties())
  })

  # --- Deelbare links (bookmarking via de URL) ---

  setBookmarkExclude(c(
    "ophalen", "thema", "budget_hover",
    "kaart_bounds", "kaart_center", "kaart_zoom", "kaart_shape_click",
    "kaart_shape_mouseover", "kaart_shape_mouseout", "kaart_click",
    "trend_gebieden", "profiel_gemeente", "ga_naar",
    "tabel_rows_current", "tabel_rows_all", "tabel_state", "tabel_search",
    "tabel_cell_clicked", "tabel_rows_selected"
  ))
  # De link bewaart de zoekvraag van het getóónde resultaat (niet wat er
  # daarna eventueel in de invoervelden is veranderd), plus de keuzelijsten
  # die pas na het ophalen gevuld zijn.
  onBookmark(function(state) {
    res <- resultaat()
    if (!is.null(res)) {
      state$values$zoek <- list(termen = res$termen, jaren = res$jaren,
                                opties = res$opties)
    }
    state$values$trend <- input$trend_gebieden
    state$values$profiel <- input$profiel_gemeente
  })
  wordt_hersteld <- FALSE
  onRestore(function(state) wordt_hersteld <<- TRUE)
  onRestored(function(state) {
    zoek <- state$values$zoek
    if (is.null(zoek)) return()
    termen <- unlist(zoek$termen)
    # Eigen termen staan niet in de keuzelijst: toevoegen, anders verdwijnen
    # ze uit het invoerveld
    eigen <- setdiff(termen, unlist(TERMEN))
    updateSelectizeInput(
      session, "termen",
      choices = c(TERMEN, if (length(eigen) > 0) list("Eigen termen" = eigen)),
      selected = termen
    )
    updateSliderInput(session, "jaren", value = unlist(zoek$jaren))
    opties <- lapply(zoek$opties, isTRUE)
    doe_ophalen(termen, unlist(zoek$jaren), opties,
                trend_selectie = unlist(state$values$trend),
                profiel_selectie = unlist(state$values$profiel))
  })

  # Bij het openen (zonder gedeelde link) meteen de standaardzoekvraag tonen;
  # die is voorberekend en dus direct beschikbaar
  eerste_keer <- observe({
    eerste_keer$destroy()
    if (!wordt_hersteld) {
      isolate(doe_ophalen(STANDAARD_TERMEN, standaard_periode(), STANDAARD_OPTIES))
    }
  })

  # Themakeuze loslaten zodra de termen niet meer bij het thema passen
  observeEvent(input$termen, {
    if (nzchar(input$thema %||% "") &&
        !setequal(input$termen, THEMASETS[[input$thema]])) {
      updateSelectInput(session, "thema", selected = "")
    }
  }, ignoreNULL = FALSE)
  onBookmarked(function(url) {
    showBookmarkUrlModal(url)
  })

  # --- Status ---

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
      tags$br(), tags$small(paste(c(
        if (res$opties$context) "cultuurcontext",
        if (res$opties$dedup) "zonder dubbele bijlagen",
        if (res$opties$woordvormen) "met woordvormen"
      ), collapse = " · ")),
      tags$br(), tags$small(
        if (identical(res$bron, "voorberekend")) {
          sprintf("Voorberekend op %s",
                  format(res$berekend_op, "%d-%m-%Y %H:%M",
                         tz = "Europe/Amsterdam"))
        } else {
          "Live opgehaald"
        }),
      if (identical(res$bron, "voorberekend") &&
          leeftijd_dagen(res) > WAARSCHUW_LEEFTIJD_DAGEN) {
        tags$div(class = "text-warning",
                 sprintf("Let op: deze gegevens zijn %d dagen oud; de nachtelijke",
                         floor(leeftijd_dagen(res))),
                 "bijwerking is waarschijnlijk mislukt.")
      },
      if (!res$inwoners_ok) {
        tags$div(class = "text-warning", "CBS-inwonersdata niet beschikbaar.")
      }
    )
  })

  # Resultaten met een kolom 'waarde' volgens de gekozen maatstaf, gesorteerd
  gerangschikt <- reactive({
    res <- resultaat()
    req(res)
    rangschik(res$per_gemeente, input$maatstaf, input$klasse)
  })

  # --- Kaart ---

  output$kaart <- renderLeaflet(basiskaart())
  # Ook tekenen als de kaart (nog) niet zichtbaar is, bv. bij een gedeelde
  # link die op een ander tabblad opent; anders gaan de kaartlagen verloren
  outputOptions(output, "kaart", suspendWhenHidden = FALSE)

  # Gemeentegrenzen: eerste keer ~2 s bij PDOK, daarna uit de schijfcache
  grenzen <- tryCatch(per_proces("grenzen", haal_gemeentegrenzen), error = function(e) {
    showNotification(paste("Gemeentegrenzen niet beschikbaar:",
                           conditionMessage(e)), type = "error", duration = 10)
    NULL
  })

  observe({
    req(grenzen)
    res <- resultaat()
    req(res)
    kaart_df <- maak_kaart_df(grenzen, res$per_gemeente, input$maatstaf)
    leafletProxy("kaart") |>
      clearShapes() |>
      clearControls() |>
      voeg_kaartlagen_toe(kaart_df, input$maatstaf)
  })

  # Zoekvak 'Ga naar gemeente': direct naar het profiel
  observeEvent(input$ga_naar, {
    req(nzchar(input$ga_naar))
    updateSelectInput(session, "profiel_gemeente", selected = input$ga_naar)
    updateTabsetPanel(session, "tabs", selected = "profiel")
  })

  observeEvent(input$kaart_shape_click, {
    res <- resultaat()
    req(res)
    rij <- res$per_gemeente |>
      filter(gemeentecode %in% input$kaart_shape_click$id)
    if (nrow(rij) == 0) {
      showNotification("Van deze gemeente is geen raadsarchief beschikbaar.",
                       type = "warning")
      return()
    }
    updateSelectInput(session, "profiel_gemeente", selected = rij$key[1])
    updateTabsetPanel(session, "tabs", selected = "profiel")
  })

  # --- Ranking ---

  ranking_tabel <- reactive({
    res <- resultaat()
    gerangschikt() |>
      transmute(
        Rang = rang,
        Gemeente = gemeente,
        Waarde = waarde,
        `Bandbreedte laag` = waarde_laag,
        `Bandbreedte hoog` = waarde_hoog,
        `Per 1.000 docs` = per_1000,
        `Per 100k inw./jaar` = per_100k,
        Totaal = totaal,
        across(all_of(res$termen)),
        `Archief (docs)` = archief,
        `Jaren met archief` = jaren_dekking,
        Inwoners = inwoners,
        Grootteklasse = klasse
      )
  })

  # Alle gemeenten, sorteerbaar en doorzoekbaar. Getallen blijven getallen
  # (goed sorteren); de opmaak (decimale komma) doet DT.
  output$tabel <- DT::renderDT({
    df <- ranking_tabel()
    shiny::validate(need(nrow(df) > 0, "Geen resultaten."))
    cijfers <- if (input$maatstaf == "absoluut") 0 else 1
    termen <- resultaat()$termen
    eenheid <- EENHEDEN[[input$maatstaf]]
    tabel <- df |>
      transmute(
        Rang, Gemeente,
        !!eenheid := Waarde,
        # 95%-interval: de echte waarde ligt met grote waarschijnlijkheid hierin
        Bandbreedte = paste0(fmt(`Bandbreedte laag`, cijfers), " – ",
                             fmt(`Bandbreedte hoog`, cijfers)),
        Treffers = Totaal,
        across(all_of(termen)),
        `Jaren met archief` = `Jaren met archief`,
        Inwoners
      )
    DT::datatable(
      tabel, rownames = FALSE, selection = "none",
      options = list(
        pageLength = 25, lengthMenu = c(25, 50, 100, 400),
        order = list(), scrollX = TRUE,
        language = list(
          search = "Zoek gemeente:", lengthMenu = "Toon _MENU_ gemeenten",
          info = "_START_–_END_ van _TOTAL_ gemeenten",
          infoEmpty = "Geen gemeenten", infoFiltered = "(van _MAX_)",
          zeroRecords = "Geen gemeente gevonden",
          paginate = list(previous = "Vorige", `next` = "Volgende"))
      )
    ) |>
      DT::formatRound(eenheid, digits = cijfers, mark = ".", dec.mark = ",") |>
      DT::formatRound(c("Treffers", termen, "Inwoners"), digits = 0,
                      mark = ".", dec.mark = ",") |>
      DT::formatRound("Jaren met archief", digits = 1, mark = ".", dec.mark = ",")
  })

  output$dl_ranking <- downloadHandler(
    filename = \() sprintf("cultuur-ranking-%s.csv", Sys.Date()),
    content = \(file) schrijf_csv(ranking_tabel(), file)
  )

  # --- Trend ---

  # Trendcijfers per gemeente; ORI-antwoorden worden al gecachet in post_json
  trend_voor <- function(key, res) {
    if (key == "NL") {
      return(res$jaren_nl |> mutate(gebied = "Nederland"))
    }
    rij <- res$per_gemeente |> filter(key == !!key)
    if (nrow(rij) != 1) return(NULL)
    # Het resultaat bevat de trends van alle gemeenten
    res$trends |> filter(key == !!key) |> select(-key) |>
      mutate(gebied = rij$gemeente)
  }

  trend_plot <- reactive({
    res <- resultaat()
    req(res, length(input$trend_gebieden) > 0)
    df <- bind_rows(lapply(input$trend_gebieden, trend_voor, res = res))
    shiny::validate(need(nrow(df) > 0 && any(df$n > 0),
                         "Geen treffers in deze periode."))
    df$gebied <- factor(df$gebied, levels = unique(df$gebied))
    plot_trend(df, res$termen, res$jaren,
               relatief = input$maatstaf != "absoluut",
               titel = sprintf("Aandacht voor %s",
                               paste(res$termen, collapse = ", ")))
  })

  output$trend <- renderPlot(trend_plot(), alt = reactive({
    res <- resultaat()
    req(res)
    sprintf(paste("Lijngrafieken per zoekterm (%s) met de aandacht per jaar",
                  "van %d tot en met %d voor: %s."),
            paste(res$termen, collapse = ", "), res$jaren[1], res$jaren[2],
            paste(input$trend_gebieden, collapse = ", "))
  }))

  output$dl_trend <- downloadHandler(
    filename = \() sprintf("cultuur-trend-%s.png", Sys.Date()),
    content = \(file) ggsave(file, trend_plot(), width = 12, height = 7,
                             dpi = 150, bg = "white")
  )

  # --- Budget (CBS Iv3), per keuze één keer ophalen ---

  # Eén keer per proces (gedeeld door alle bezoekers); normaal uit de
  # nachtelijke voorberekening, anders live bij het CBS. Mislukt het, dan
  # wordt het een minuut niet opnieuw geprobeerd.
  lasten_voor <- function(keuze) {
    tryCatch(
      per_proces(paste0("iv3_", keuze), \() haal_cultuurlasten(keuze)),
      error = function(e) {
        showNotification(paste("CBS-budgetdata ophalen mislukt:",
                               conditionMessage(e)),
                         type = "error", duration = 10)
        NULL
      }
    )
  }

  # --- Gemeenteprofiel ---

  profiel_rij <- reactive({
    res <- resultaat()
    shiny::validate(need(res, "Haal eerst live data op."))
    req(input$profiel_gemeente)
    rij <- res$per_gemeente |> filter(key == input$profiel_gemeente)
    req(nrow(rij) == 1)
    rij
  })

  output$profiel_kerncijfers <- renderUI({
    rij <- profiel_rij()
    # Rang onder alle gemeenten, en binnen de eigen grootteklasse
    rang <- function(kolom) {
      maatstaf <- if (kolom == "per_1000") "relatief" else "inwoners"
      pg <- resultaat()$per_gemeente
      tekst <- rang_tekst(rangschik(pg, maatstaf), rij$key)
      klasse <- grootteklasse(rij$inwoners)
      binnen <- if (!is.na(klasse)) {
        rangschik(pg, maatstaf, klasse)
      }
      if (!is.null(binnen) && !is.na(binnen$rang[binnen$key == rij$key][1])) {
        tekst <- paste0(tekst, "; ", rang_tekst(binnen, rij$key),
                        " bij gemeenten met ", klasse)
      }
      tekst
    }
    # Uit het procesgeheugen of de nachtelijke voorberekening; meestal direct
    budget <- lasten_voor(IV3_KEUZES[[1]])
    euro <- if (!is.null(budget)) {
      budget$cultuur_per_inw[budget$gemeentecode %in% rij$gemeentecode][1]
    }
    blok <- function(waarde, uitleg) {
      div(class = "kerncijfer", div(class = "waarde", waarde),
          div(class = "uitleg", uitleg))
    }
    div(class = "kerncijfers",
        blok(fmt(rij$per_1000),
             paste0("per 1.000 raadsdocumenten (bandbreedte ",
                    fmt(rij$per_1000_laag), "–", fmt(rij$per_1000_hoog),
                    ") · ", rang("per_1000"))),
        blok(fmt(rij$per_100k),
             paste0("per 100.000 inwoners per jaar (bandbreedte ",
                    fmt(rij$per_100k_laag), "–", fmt(rij$per_100k_hoog),
                    ") · ", rang("per_100k"))),
        blok(fmt(rij$totaal, 0),
             if (is.na(rij$jaren_dekking)) "documenten met een treffer"
             else sprintf("documenten met een treffer · archief in %s van %s jaar",
                          fmt(rij$jaren_dekking),
                          fmt(periode_jaren(resultaat()$jaren)))),
        blok(fmt(rij$inwoners, 0), "inwoners (CBS)"),
        if (!is.null(euro) && !is.na(euro)) {
          blok(paste0("€", fmt(euro, 0)),
               paste("cultuur per inwoner,", names(IV3_KEUZES)[1]))
        })
  })

  # Een paar zinnen in gewone taal (zie verhaal_gemeente in R/uitleg.R)
  output$profiel_verhaal <- renderUI({
    rij <- profiel_rij()
    res <- resultaat()
    zinnen <- verhaal_gemeente(
      rij, res$per_gemeente, trend_voor(rij$key, res),
      budget = lasten_voor(IV3_KEUZES[[1]]),
      budget_label = names(IV3_KEUZES)[1]
    )
    req(length(zinnen) > 0)
    div(class = "verhaal", lapply(zinnen, tags$p))
  })

  output$profiel_trend <- renderPlot({
    rij <- profiel_rij()
    res <- resultaat()
    df <- bind_rows(trend_voor(rij$key, res), trend_voor("NL", res))
    shiny::validate(need(nrow(df) > 0, "Geen trendgegevens."))
    df$gebied <- factor(df$gebied, levels = unique(df$gebied))
    # Alleen het totaal van de gekozen termen: past beter in de halve breedte
    plot_trend(df, res$termen, res$jaren,
               relatief = input$maatstaf != "absoluut",
               titel = "Aandacht door de jaren", alleen_totaal = TRUE)
  }, alt = reactive(sprintf(
    "Lijngrafiek: aandacht per jaar in %s naast heel Nederland.",
    profiel_rij()$gemeente)))

  output$profiel_budget <- renderPlot({
    rij <- profiel_rij()
    req(input$tabs == "profiel")
    lasten <- bind_rows(lapply(IV3_KEUZES, lasten_voor))
    shiny::validate(need(nrow(lasten) > 0, "Budgetgegevens niet beschikbaar."),
                    need(!is.na(rij$gemeentecode), "Geen CBS-gemeentecode."))
    plot_budget_trend(lasten, rij$gemeentecode, rij$gemeente)
  }, alt = reactive(sprintf(
    "Staafdiagram: lasten voor cultuur per inwoner in %s naast de mediaan van alle gemeenten, 2023 tot en met 2026.",
    profiel_rij()$gemeente)))

  output$profiel_fragmenten <- renderUI({
    rij <- profiel_rij()
    res <- resultaat()
    frag <- tryCatch(
      haal_fragmenten(rij$ruw[[1]], res$termen, res$jaren, res$opties),
      error = function(e) {
        tags$p(class = "text-danger",
               paste("Fragmenten ophalen mislukt:", conditionMessage(e)))
      }
    )
    if (inherits(frag, "shiny.tag")) return(frag)
    if (nrow(frag) == 0) return(helpText("Geen vermeldingen gevonden."))
    tagList(lapply(seq_len(nrow(frag)), function(i) {
      f <- frag[i, ]
      link <- veilige_link(f$link)
      div(class = "fragment",
          div(class = "meta", f$datum, " · ",
              if (nzchar(link)) tags$a(href = link, target = "_blank", f$titel)
              else f$titel),
          if (f$privacy) {
            tags$p(class = "text-muted", tags$small(
              "Geen fragment: in dit soort stuk staan vaak gegevens van burgers."))
          } else if (nzchar(f$fragmenten)) {
            # Zelf ge-escaped; alleen onze <mark>-tags zijn HTML
            tags$p(HTML(markeer_html(f$fragmenten)))
          })
    }))
  })

  # --- Aandacht vs. budget ---

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
    lasten <- lasten_voor(input$budget_keuze)
    shiny::validate(need(lasten, "Budgetdata niet beschikbaar."))
    df <- maak_budget_df(res$per_gemeente, lasten, aandacht_kolom())
    shiny::validate(need(nrow(df) >= 5, "Te weinig gemeenten om te vergelijken."))
    df
  })

  budget_plot <- reactive({
    plot_budget(budget_df(), resultaat()$jaren, aandacht_label(),
                names(IV3_KEUZES)[IV3_KEUZES == input$budget_keuze])
  })

  output$budget_plot <- renderPlot(budget_plot(), alt = reactive(sprintf(paste(
    "Spreidingsdiagram van %d gemeenten: cultuurlasten per inwoner tegen de",
    "aandacht in de raad, ingedeeld in vier profielen. De tabel hieronder",
    "toont de grootste verschillen."), nrow(budget_df()))))

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

  output$dl_budget_png <- downloadHandler(
    filename = \() sprintf("cultuur-aandacht-vs-budget-%s.png", Sys.Date()),
    content = \(file) ggsave(file, budget_plot(), width = 12, height = 8,
                             dpi = 150, bg = "white")
  )
  output$dl_budget_csv <- downloadHandler(
    filename = \() sprintf("cultuur-aandacht-vs-budget-%s.csv", Sys.Date()),
    content = \(file) schrijf_csv(
      budget_df() |>
        transmute(Gemeente = gemeente, Gemeentecode = gemeentecode,
                  Inwoners = inwoners, Aandacht = aandacht,
                  `Cultuur euro per inwoner` = cultuur_per_inw,
                  Profiel = profiel,
                  `Percentiel aandacht` = 100 * rang_aandacht,
                  `Percentiel budget` = 100 * rang_budget),
      file)
  )

  # --- Documenten ---

  output$documenten <- renderTable({
    res <- resultaat()
    req(res)
    shiny::validate(need(nrow(res$docs) > 0, "Geen documenten."))
    res$docs |>
      mutate(link = veilige_link(link),
             titel = ifelse(
               nzchar(link),
               sprintf('<a href="%s" target="_blank">%s</a>',
                       htmltools::htmlEscape(link, attribute = TRUE),
                       htmltools::htmlEscape(titel)),
               htmltools::htmlEscape(titel))) |>
      select(Gemeente = gemeente, Datum = datum, Document = titel)
  }, striped = TRUE, sanitize.text.function = identity)

  output$dl_docs <- downloadHandler(
    filename = \() sprintf("cultuur-documenten-%s.csv", Sys.Date()),
    content = \(file) schrijf_csv(
      resultaat()$docs |>
        mutate(link = veilige_link(link)) |>
        select(Gemeente = gemeente, Datum = datum, Titel = titel, Link = link),
      file)
  )
}

shinyApp(ui, server, enableBookmarking = "url")
