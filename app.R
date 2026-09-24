# =============================================================================
# Kunst & cultuur in de gemeenteraad — dashboard
# Live data uit Open Raadsinformatie / OpenBesluitvorming.nl + CBS
#
# Starten:  shiny::runApp("pad/naar/deze/map")
# Hulpfuncties staan in R/ en worden door Shiny automatisch geladen.
# =============================================================================

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
