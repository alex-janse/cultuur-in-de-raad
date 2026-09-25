# --- Uitleg en verhaal per gemeente ---------------------------------------------

procent <- function(x) paste0(fmt(x, 0), "%")

# Richting van de aandacht: de laatste twee volledige jaren tegenover de
# eerste twee jaren van de periode (per 1.000 raadsdocumenten). NULL als er
# te weinig gegevens zijn voor een uitspraak.
trend_richting <- function(trend, jaar_nu = huidig_jaar()) {
  t <- trend |>
    filter(term == "__alle__", jaar < jaar_nu, archief >= MIN_DOCS_DEKKING) |>
    arrange(jaar) |>
    mutate(p = 1000 * n / archief)
  if (nrow(t) < 4 || sum(t$n) < 30) return(NULL)
  begin <- mean(head(t$p, 2))
  eind <- mean(tail(t$p, 2))
  if (begin == 0) return(NULL)
  verandering <- eind / begin - 1
  list(
    verandering = verandering,
    tekst = if (verandering > 0.2) "toegenomen" else if (verandering < -0.2)
      "afgenomen" else "ongeveer gelijk gebleven",
    van = paste(head(t$jaar, 1), head(t$jaar, 2)[2], sep = "–"),
    tot = paste(tail(t$jaar, 2)[1], tail(t$jaar, 1), sep = "–")
  )
}

# Een paar zinnen in gewone taal over één gemeente. Voorzichtig geformuleerd:
# bij weinig treffers geen vergelijking, en altijd 'in verhouding'.
verhaal_gemeente <- function(rij, per_gemeente, trend, budget = NULL,
                             budget_label = NULL) {
  naam <- rij$gemeente
  zinnen <- character()

  if (isTRUE(rij$weinig_treffers) || is.na(rij$per_1000)) {
    zinnen <- c(zinnen, sprintf(paste(
      "In %s komen de gekozen termen in %s documenten voor. Dat is te weinig",
      "om betrouwbaar met andere gemeenten te vergelijken."),
      naam, fmt(rij$totaal, 0)))
  } else {
    p <- percentiel(rangschik(per_gemeente, "relatief"), rij$key)
    klasse <- grootteklasse(rij$inwoners)
    p_klasse <- if (!is.na(klasse)) {
      percentiel(rangschik(per_gemeente, "relatief", klasse), rij$key)
    }
    vergelijk <- function(p) {
      if (p >= 50) sprintf("vaker dan %s", procent(p))
      else sprintf("minder vaak dan %s", procent(100 - p))
    }
    zin <- sprintf("%s bespreekt deze thema's in verhouding %s van de gemeenten",
                   naam, vergelijk(p))
    if (!is.null(p_klasse) && !is.na(p_klasse)) {
      zin <- sprintf("%s, en %s van de gemeenten met %s", zin,
                     vergelijk(p_klasse), klasse)
    }
    zinnen <- c(zinnen, paste0(zin, "."))
  }

  richting <- trend_richting(trend)
  if (!is.null(richting)) {
    zinnen <- c(zinnen, sprintf(
      "De aandacht is %s: van %s naar %s %s.",
      richting$tekst, richting$van, richting$tot,
      if (abs(richting$verandering) > 0.2)
        sprintf("(%s%s)", if (richting$verandering > 0) "+" else "−",
                procent(abs(100 * richting$verandering)))
      else ""
    ) |> sub(pattern = " \\.$", replacement = "."))
  }

  if (!is.null(budget) && !is.na(rij$gemeentecode)) {
    euro <- budget$cultuur_per_inw[budget$gemeentecode == rij$gemeentecode][1]
    mediaan <- stats::median(budget$cultuur_per_inw, na.rm = TRUE)
    if (length(euro) == 1 && !is.na(euro)) {
      bron <- if (is.null(budget_label)) "" else
        sprintf("Volgens de %s ", tolower(budget_label))
      zinnen <- c(zinnen, sprintf(
        "%s%s de gemeente €%s per inwoner aan cultuur uit; de mediaan van alle gemeenten is €%s.",
        bron, if (nzchar(bron)) "gaf" else "Gaf", fmt(euro, 0), fmt(mediaan, 0)))
    }
  }
  zinnen
}

# Tabblad 'Uitleg': hoe lees je het dashboard, in gewone taal
uitleg_ui <- function() {
  div(
    class = "uitleg-tekst",
    h3("Wat laat dit dashboard zien?"),
    p("Hoe vaak Nederlandse gemeenteraden het hebben over kunst- en",
      "cultuurthema's zoals amateurkunst, cultuurbeoefening en",
      "cultuureducatie. We tellen raadsdocumenten (voorstellen, moties,",
      "verslagen, bijlagen) waarin een zoekterm voorkomt, via",
      tags$a(href = "https://openbesluitvorming.nl", target = "_blank",
             "OpenBesluitvorming.nl"), ". Daarnaast staan inwonertallen en",
      "cultuurbudgetten van het CBS."),

    h3("Hoe lees je de cijfers?"),
    tags$ul(
      tags$li(tags$b("Per 1.000 raadsdocumenten"), " (standaard): in hoeveel",
              "van elke 1.000 documenten van de gemeente een term voorkomt. Zo",
              "maakt het niet uit hoe groot het archief is."),
      tags$li(tags$b("Per 100.000 inwoners per jaar"), ": handig om",
              "gemeenten van verschillende grootte te vergelijken. Alleen voor",
              "gemeenten vanaf 20.000 inwoners."),
      tags$li(tags$b("Absoluut"), ": het aantal documenten. Grote gemeenten",
              "staan dan vanzelf bovenaan.")
    ),

    h3("Bandbreedte en rang"),
    p("Bij elk cijfer staat een bandbreedte: de echte waarde ligt met grote",
      "waarschijnlijkheid daartussen. Bij weinig treffers is die breed.",
      "Overlappen de bandbreedtes van twee gemeenten, dan is het verschil",
      "tussen hen niet betekenisvol. Gemeenten met minder dan",
      MIN_TREFFERS_RANG, "treffers krijgen geen rang: hun plek is vooral toeval.",
      "Met 'Vergelijk met' boven de ranking kies je gemeenten van dezelfde",
      "grootte; dat is vaak een eerlijkere vergelijking."),

    h3("Hoe wordt er geteld?"),
    tags$ul(
      tags$li(tags$b("Alleen in cultuurcontext"), ": een term die niet zelf",
              "over cultuur gaat (zoals 'talentontwikkeling' of",
              "'combinatiefunctionaris') telt alleen als er binnen",
              CONTEXT_AFSTAND, "woorden een cultuurwoord staat. Anders gaat",
              "het vaak over sport of onderwijs."),
      tags$li(tags$b("Dubbele bijlagen samenvoegen"), ": dezelfde bijlage",
              "hangt vaak bij meerdere agendapunten en telt dan één keer."),
      tags$li(tags$b("Jaren met archief"), ": niet elke gemeente heeft alle",
              "jaren in het archief. We delen alleen door de jaren waarin er",
              "wél documenten zijn."),
      tags$li(tags$b("Het lopende jaar"), " is nog niet compleet; in de",
              "grafieken staat het als open punt met stippellijn.")
    ),

    h3("Waar moet je op letten?"),
    tags$ul(
      tags$li("Aandacht is geen beleid: een vermelding kan in een besluit",
              "staan, maar ook in een bijlage of een verworpen motie. Het",
              "gemeenteprofiel laat de zinnen zien, zodat je dat kunt nagaan."),
      tags$li("Niet alle gemeenten hebben een archief in OpenBesluitvorming;",
              "die zijn grijs op de kaart."),
      tags$li("Het cultuurbudget is een indicatie (CBS, inclusief",
              "afschrijvingen en verrekeningen)."),
      tags$li("Dit is een testversie: de betrouwbaarheid per zoekterm wordt",
              "nog met een steekproef gecontroleerd.")
    ),

    h3("Privacy"),
    p(PRIVACY_TEKST)
  )
}
