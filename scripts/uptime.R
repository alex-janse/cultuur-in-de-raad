# =============================================================================
# Controle van de online app (draait dagelijks in GitHub Actions, zie
# .github/workflows/uptime.yml; lokaal: Rscript scripts/uptime.R [url])
#
# Opent de app in een headless Chrome en controleert dat binnen twee minuten:
#   - het resultaat van de standaardweergave verschijnt;
#   - de kaart getekend is;
#   - de app niet waarschuwt voor oude of ontbrekende gegevens.
# Stopt met een fout als iets daarvan niet klopt; de workflow wordt dan rood.
# Doet geen verzoeken aan de API van OpenBesluitvorming (alleen voorberekend).
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
url <- if (length(args) > 0) args[1] else
  "https://01a0d57f-3a32-a4ab-9874-1761a480a727.share.connect.posit.cloud/"

# Chrome start op een GitHub-runner soms trager dan de standaard 10 s van
# chromote ("Chrome debugging port not open", 1 en 8 oktober 2026). Daarom
# meer tijd en een paar pogingen, zodat de controle niet rood wordt zonder dat
# de app zelf bekeken is.
options(chromote.timeout = 60)
b <- NULL
for (poging in 1:3) {
  b <- tryCatch(chromote::ChromoteSession$new(width = 1400, height = 1000),
                error = function(e) {
                  message("Chrome starten mislukt (poging ", poging, "): ",
                          conditionMessage(e))
                  NULL
                })
  if (!is.null(b)) break
  Sys.sleep(5)
}
if (is.null(b)) stop("Chrome kon niet starten op de runner; de app is niet gecontroleerd.")
b$Emulation$setFocusEmulationEnabled(enabled = TRUE)

# De app kan in een iframe van Connect Cloud staan
doc <- paste0("(function(){var d=document;var f=d.querySelector('iframe');",
              "if(f){try{if(f.contentDocument&&f.contentDocument.body)d=f.contentDocument}",
              "catch(e){}}return d})()")
waarde <- function(expr) {
  js <- sprintf("(function(){var d=%s;try{return %s}catch(e){return null}})()", doc, expr)
  tryCatch(b$Runtime$evaluate(js)$result$value, error = function(e) NULL)
}

t0 <- Sys.time()
b$Page$navigate(url, wait_ = FALSE)
# Niet falen op het laadmoment zelf; hieronder telt alleen of de app verschijnt
tryCatch(b$Page$loadEventFired(timeout_ = 60), error = function(e) NULL)

status <- ""
kaart <- 0
for (i in 1:240) {
  status <- waarde("d.querySelector('#status').innerText") %||% ""
  kaart <- waarde("d.querySelectorAll('#kaart path.leaflet-interactive').length") %||% 0
  if (nchar(status) > 50 && kaart > 100) break
  Sys.sleep(0.5)
}
duur <- as.numeric(Sys.time() - t0, units = "secs")
b$close()

message(sprintf("Na %.1f s: %d kaartvlakken", duur, kaart))
message("Status: ", gsub("\\s+", " ", status))

problemen <- c(
  if (nchar(status) <= 50) "geen resultaat binnen twee minuten",
  if (kaart <= 100) "de kaart is niet getekend",
  if (grepl("Let op", status)) "de app meldt dat de gegevens oud zijn",
  if (grepl("niet beschikbaar", status)) "de app meldt ontbrekende gegevens"
)
if (length(problemen) > 0) {
  stop("De online app werkt niet goed: ", paste(problemen, collapse = "; "))
}
message("In orde.")
