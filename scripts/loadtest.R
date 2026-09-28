# =============================================================================
# Eenvoudige loadtest: N echte browsersessies tegelijk tegen de online app
#
#   Rscript scripts/loadtest.R [aantallen ...]     bv. 1 5 10 20
#
# Elke sessie opent de app (resultaat + kaart), gaat naar 'Aandacht vs.
# budget' en daarna naar 'Trend'; per stap de mediaan en de traagste tijd.
# Alle sessies starten op dezelfde seconde: een worstcasetest. Bij 20+
# sessies kan de eigen computer de meting vertekenen. Doet geen verzoeken aan
# de API van OpenBesluitvorming (alleen voorberekende data).
# =============================================================================
url <- "https://01a0d57f-3a32-a4ab-9874-1761a480a727.share.connect.posit.cloud/"
`%||%` <- function(a, b) if (is.null(a)) b else a
niveaus <- as.integer(commandArgs(TRUE))
if (length(niveaus) == 0) niveaus <- c(1L, 5L, 10L, 20L, 25L)

# JavaScript dat in het app-document werkt (ook als de app in een iframe staat)
doc <- "(function(){var d=document;var f=d.querySelector('iframe');if(f){try{if(f.contentDocument&&f.contentDocument.body)d=f.contentDocument}catch(e){}}return d})()"
check <- function(expr) sprintf("(function(){var d=%s;try{return %s}catch(e){return false}})()", doc, expr)
KLAAR <- list(
  start  = check("d.querySelector('#status')&&d.querySelector('#status').innerText.length>50&&d.querySelectorAll('#kaart path.leaflet-interactive').length>100"),
  budget = check("!!d.querySelector('#budget_plot img')&&d.querySelector('#budget_plot img').naturalWidth>0"),
  trend  = check("!!d.querySelector('#trend img')&&d.querySelector('#trend img').naturalWidth>0")
)
KLIK <- list(
  budget = check("(d.querySelector('a[data-value=\"budget\"]').click(),true)"),
  trend  = check("(d.querySelector('a[data-value=\"trend\"]').click(),true)")
)
LOS <- check("!!d.querySelector('#shiny-disconnected-overlay')")

js <- function(s, x) tryCatch(isTRUE(s$Runtime$evaluate(x, timeout_ = 10)$result$value), error = function(e) FALSE)

# Wacht tot alle sessies een stap af hebben; geeft seconden per sessie (NA = time-out)
wacht_allen <- function(sessies, expr, t0, max = 180) {
  tijd <- rep(NA_real_, length(sessies))
  repeat {
    for (i in which(is.na(tijd))) if (js(sessies[[i]], expr)) tijd[i] <- as.numeric(Sys.time() - t0, units = "secs")
    if (!anyNA(tijd) || as.numeric(Sys.time() - t0, units = "secs") > max) break
    Sys.sleep(0.5)
  }
  tijd
}

samenvat <- function(t) sprintf("mediaan %4.1fs, traagste %4.1fs, mislukt %d",
                                median(t, na.rm = TRUE), max(t, na.rm = TRUE), sum(is.na(t)))

for (n in niveaus) {
  sessies <- lapply(seq_len(n), \(i) chromote::ChromoteSession$new(width = 1300, height = 900))
  # Anders geldt elk tabblad behalve het laatste als verborgen en tekent Shiny niets
  for (s in sessies) s$Emulation$setFocusEmulationEnabled(enabled = TRUE)
  t0 <- Sys.time()
  for (s in sessies) s$Page$navigate(url, wait_ = FALSE)
  t_start <- wacht_allen(sessies, KLAAR$start, t0)
  uitslag <- list(start = t_start)
  for (stap in c("budget", "trend")) {
    t1 <- Sys.time()
    for (s in sessies) js(s, KLIK[[stap]])
    uitslag[[stap]] <- wacht_allen(sessies, KLAAR[[stap]], t1, max = 120)
  }
  los <- sum(vapply(sessies, js, logical(1), LOS))
  cat(sprintf("\n== %d gelijktijdige bezoekers (verbinding verloren: %d)\n", n, los))
  for (stap in names(uitslag)) cat(sprintf("  %-7s %s\n", stap, samenvat(uitslag[[stap]])))
  for (s in sessies) try(s$close(), silent = TRUE)
  Sys.sleep(20)
}
