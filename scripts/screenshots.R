# =============================================================================
# Schermafbeeldingen voor README en projectpagina (docs/img/)
#
#   Rscript scripts/screenshots.R [url]
#
# Zonder url start het script de app zelf (poort 8765) met de voorberekende
# data van de tak 'data'. Instellingen gaan via een deelbare link (bookmark),
# net als bij een gebruiker. Doet geen verzoeken aan de API van
# OpenBesluitvorming (alles is voorberekend).
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
eigen_app <- length(args) == 0
url <- if (eigen_app) "http://127.0.0.1:8765/" else args[1]
map <- file.path("docs", "img")

if (eigen_app) {
  app <- callr::r_bg(function() shiny::runApp(".", port = 8765, launch.browser = FALSE))
  on.exit(app$kill(), add = TRUE)
  for (i in 1:60) {
    if (!inherits(try(readLines(url, warn = FALSE), silent = TRUE), "try-error")) break
    Sys.sleep(1)
  }
}

# Deelbare link: invoer (thema, periode, tabblad) en de gekozen gemeenten
link <- function(tab, trend = c("NL", "tilburg", "utrecht"), profiel = "tilburg") {
  json <- \(x) utils::URLencode(jsonlite::toJSON(x, auto_unbox = TRUE), reserved = TRUE)
  paste0(url, "?_inputs_",
         "&thema_vast=", json("Standaard"), "&blok=", json(STANDAARD_BLOK),
         "&maatstaf=", json("relatief"), "&tabs=", json(tab),
         "&_values_", "&trend=", json(trend), "&profiel=", json(profiel))
}
STANDAARD_BLOK <- "2022-2025"

b <- chromote::ChromoteSession$new(width = 1440, height = 1500)
b$Emulation$setFocusEmulationEnabled(enabled = TRUE)
js <- function(x) isTRUE(b$Runtime$evaluate(x)$result$value)
wacht <- function(expr, max = 90) {
  for (i in seq_len(max * 2)) { if (js(expr)) return(invisible(TRUE)); Sys.sleep(0.5) }
  stop("Time-out bij wachten op: ", expr)
}
# Een grafiek is klaar als het plaatje er is en Shiny hem niet meer bijwerkt
plaatje <- paste0("(function(s){var e=document.querySelector(s),i=e&&e.querySelector('img');",
                  "return !!i&&i.naturalWidth>0&&!e.classList.contains('recalculating')})")
open <- function(tab) {
  b$Page$navigate(link(tab))
  b$Page$loadEventFired()
  wacht("!!document.querySelector('#status')&&document.querySelector('#status').innerText.length>50")
}

open("kaart")
wacht("document.querySelectorAll('#kaart path.leaflet-interactive').length>100")
Sys.sleep(2)
b$screenshot(file.path(map, "kaart.png"), cliprect = c(0, 0, 1440, 1500))

open("trend")
wacht("($('#trend_gebieden').val()||[]).length>=3")
Sys.sleep(1)
wacht(sprintf("%s('#trend')", plaatje))
Sys.sleep(1)
b$screenshot(file.path(map, "trend.png"), selector = "#trend")

open("profiel")
wacht(sprintf("%s('#profiel_trend')&&%s('#profiel_budget')", plaatje, plaatje))
wacht("document.querySelectorAll('#profiel_fragmenten .fragment').length>0")
Sys.sleep(1)
b$screenshot(file.path(map, "profiel.png"), selector = ".tab-pane[data-value='profiel']")

open("budget")
wacht(sprintf("%s('#budget_plot')", plaatje))
Sys.sleep(1)
b$screenshot(file.path(map, "budget.png"), selector = "#budget_plot")

b$close()
message("Opgeslagen in ", map)
