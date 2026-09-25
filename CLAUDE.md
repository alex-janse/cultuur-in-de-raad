# Cultuur in de raad

R Shiny-dashboard over kunst- en cultuurthema's in Nederlandse gemeenteraden
(OpenBesluitvorming, CBS, PDOK). Zie README.md voor opzet en methode.

- Online app: https://01a0d57f-3a32-a4ab-9874-1761a480a727.share.connect.posit.cloud/
  (Posit Connect Cloud, publiceert vanaf `main`)
- Repository: https://github.com/alex-janse/cultuur-in-de-raad (openbaar)
- Projectpagina: https://alex-janse.github.io/cultuur-in-de-raad/ (`docs/`)
- Nachtelijke voorberekening: `.github/workflows/voorbereken.yml` schrijft naar de tak `data`

## Werkafspraken

- Commits alleen met het noreply-adres `242792222+alex-janse@users.noreply.github.com`,
  nooit met het privé-mailadres.
- Nieuwe bestanden in `R/` of nieuwe pakketten: `manifest.json` opnieuw maken met
  `rsconnect::writeManifest(appFiles = c("app.R", list.files("R", full.names = TRUE)))`,
  anders neemt Connect Cloud ze niet mee.
- Tests: `Rscript -e "testthat::test_dir('tests/testthat')"`.
- De API van OpenBesluitvorming heeft een limiet op het aantal verzoeken (HTTP 429, ~10 min
  blokkade). Test zuinig; zware tests niet vaak achter elkaar draaien.
- R-code met backslashes niet via een Bash-heredoc schrijven (`\\` wordt `\`);
  gebruik de Write/Edit-tools.

## Actiepunten voor later

- [ ] Open State Foundation mailen: melden dat hun data (OpenBesluitvorming /
      Open Raadsinformatie) in een openbare app wordt gebruikt, vragen of dat mag,
      wat de limieten van de API zijn en hoe ze naamsvermelding willen.
      Nog niet versturen: de gebruiker geeft aan wanneer.
- [ ] Validatie-steekproef (ROADMAP 1.7): volledige steekproef trekken met
      `Rscript scripts/validatie.R steekproef 50` (~12 verzoeken; niet vlak na andere
      zware tests i.v.m. HTTP 429), laten beoordelen door iemand met kennis van het veld,
      daarna `scripts/validatie.R bereken <csv>` en de precisie per term in README en
      uitleg zetten. Wacht op de gebruiker.
- [ ] Telefoonweergave controleren en verbeteren (smalle zijbalk, kaarthoogte,
      brede tabellen); geparkeerd op verzoek van de gebruiker.
- [ ] Huisstijl: alleen kleuren en lettertypen van LKCA (Barlow/Heebo als vrije
      vervangers van DIN). Géén logo of naam LKCA zonder expliciet akkoord.
