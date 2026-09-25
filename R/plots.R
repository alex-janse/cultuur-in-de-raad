# --- Grafieken ---------------------------------------------------------------
# Losse functies zodat app, tests en README-afbeeldingen dezelfde grafiek maken.

ALLE_TERMEN_LABEL <- "Alle gekozen termen"

thema_dashboard <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", hjust = 0))
}

# df: gebied, jaar, term, n, archief (term '__alle__' = minstens één term).
# Eén paneel per term met een eigen y-as, zodat kleine termen zichtbaar zijn.
# alleen_totaal = TRUE: één paneel met 'minstens één van de termen'.
plot_trend <- function(df, termen, jaren, relatief = TRUE, titel = NULL,
                       alleen_totaal = FALSE) {
  if (alleen_totaal) {
    df <- df |> filter(term == "__alle__")
    termen <- ALLE_TERMEN_LABEL
  }
  df <- df |>
    mutate(
      term = ifelse(term == "__alle__", ALLE_TERMEN_LABEL, term),
      # jaren met heel weinig documenten geven wilde uitschieters
      waarde = if (relatief) ifelse(archief >= 50, 1000 * n / archief, NA_real_)
               else n
    )
  if (length(termen) == 1 && !alleen_totaal) {
    df <- df |> filter(term != ALLE_TERMEN_LABEL)
  }
  volgorde <- c(if (length(termen) > 1) ALLE_TERMEN_LABEL, termen)
  df$term <- factor(df$term, levels = volgorde)

  # Het lopende jaar is nog niet compleet: gestippelde lijn en open punt, zodat
  # een (vooral absolute) daling niet als echte daling wordt gelezen
  jaar_nu <- huidig_jaar()
  df$lopend <- df$jaar == jaar_nu
  met_lopend <- any(df$lopend)

  stap <- max(1, ceiling((jaren[2] - jaren[1]) / 8))
  ggplot(df, aes(jaar, waarde, colour = gebied)) +
    geom_line(data = \(d) d[d$jaar < jaar_nu, ], linewidth = 1, na.rm = TRUE) +
    geom_line(data = \(d) d[d$jaar >= jaar_nu - 1, ], linewidth = 1,
              linetype = "22", na.rm = TRUE) +
    geom_point(aes(shape = lopend), size = 1.8, stroke = 1, fill = "white",
               na.rm = TRUE) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 21), guide = "none") +
    facet_wrap(~term, scales = "free_y") +
    scale_x_continuous(breaks = seq(jaren[1], jaren[2], by = stap)) +
    scale_y_continuous(labels = \(x) fmt(x, if (relatief) 1 else 0),
                       limits = c(0, NA)) +
    scale_colour_brewer(palette = "Dark2") +
    labs(
      title = titel, x = NULL, colour = NULL,
      y = if (relatief) "Documenten per 1.000 raadsdocumenten"
          else "Aantal documenten",
      caption = if (met_lopend) {
        sprintf("Open punt en stippellijn: %d is nog niet compleet.", jaar_nu)
      }
    ) +
    thema_dashboard()
}

# df: gemeente, inwoners, aandacht, cultuur_per_inw, verschil, profiel
plot_budget <- function(df, jaren, aandacht_label, budget_label) {
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
    scale_colour_manual(values = PROFIELKLEUREN) +
    labs(
      title = "Praat de raad over cultuur in verhouding tot wat de gemeente eraan uitgeeft?",
      subtitle = sprintf("%d gemeenten · %d–%d · Spearman-correlatie: %s",
                         nrow(df), jaren[1], jaren[2], fmt(rho, 2)),
      x = sprintf("Lasten cultuur per inwoner, %s (logaritmische schaal)",
                  budget_label),
      y = aandacht_label, colour = NULL
    ) +
    thema_dashboard() +
    theme(plot.title = element_text(size = 14, face = "bold"))
}

PROFIELKLEUREN <- c(
  "Veel aandacht, weinig budget" = "#c2378f",
  "Veel budget, weinig aandacht" = "#2b7bba",
  "Veel aandacht, veel budget"   = "#5a3e8c",
  "Weinig aandacht, weinig budget" = "grey55"
)

# Aandacht + budget per gemeente -> percentielrangen en profiel
maak_budget_df <- function(per_gemeente, lasten, aandacht_kolom) {
  df <- per_gemeente |>
    filter(!is.na(inwoners), inwoners >= MIN_INWONERS) |>
    inner_join(lasten |> select(gemeentecode, cultuur_per_inw),
               by = "gemeentecode") |>
    mutate(aandacht = .data[[aandacht_kolom]]) |>
    filter(!is.na(aandacht), !is.na(cultuur_per_inw))
  if (nrow(df) == 0) return(df)

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
}

# Cultuurlasten per inwoner van één gemeente door de jaren, naast de mediaan
# van alle gemeenten. lasten: gebonden uitkomsten van haal_cultuurlasten().
plot_budget_trend <- function(lasten, code, naam) {
  df <- lasten |>
    group_by(keuze, jaar) |>
    summarise(Mediaan = median(cultuur_per_inw, na.rm = TRUE),
              gemeente = cultuur_per_inw[gemeentecode == code][1],
              .groups = "drop") |>
    tidyr::pivot_longer(c(gemeente, Mediaan), names_to = "reeks",
                        values_to = "euro") |>
    mutate(reeks = ifelse(reeks == "gemeente", naam, "Mediaan alle gemeenten"),
           soort = ifelse(grepl("rekening", keuze), "jaarrekening", "begroting"),
           label = paste(jaar, soort))

  ggplot(df, aes(label, euro, fill = reeks)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75, na.rm = TRUE) +
    geom_text(aes(label = paste0("€", fmt(euro, 0))), na.rm = TRUE,
              position = position_dodge(width = 0.8), vjust = -0.4, size = 3.6) +
    scale_fill_manual(values = setNames(c("#c2378f", "grey65"),
                                        c(naam, "Mediaan alle gemeenten"))) +
    scale_y_continuous(labels = \(x) paste0("€", fmt(x, 0)),
                       expand = expansion(mult = c(0, 0.12))) +
    labs(title = "Lasten cultuur per inwoner", x = NULL, y = NULL, fill = NULL) +
    thema_dashboard(base_size = 13)
}
