# Roadmap: Cultuur in de raad

## In het kort
Het dashboard werkt en staat online. Een kritische code-review vond **44 punten**.
De belangrijkste:

1. **Eén bezoeker kan de app voor iedereen laten vastlopen.** Een zoekvraag blokkeert
   het hele R-proces, en bij een API-blokkade (HTTP 429) kan dat minutenlang duren.
2. **Na een code-update kan de app crashen op de nachtelijke data.** De bestanden op
   de data-tak worden zonder controle ingelezen.
3. **De hoofdmaatstaf is waarschijnlijk scheef.** Het samenvoegen van dubbele bijlagen
   op bestandsgrootte werkt in grote archieven ook op de noemer, waardoor grote
   gemeenten te hoog uitkomen.
4. **"Per inwoner" en de drempels rekenen met verkeerde jaren.** Het lopende jaar telt
   als heel jaar, en jaren zonder archief tellen mee.
5. **Er draaien geen automatische tests**, en de nachtelijke taak publiceert zonder
   controle.

De roadmap loopt in zes fasen, van "eerst repareren" naar "uitbreiden". Elke fase
wordt een eigen reeks commits die los online kan.

Het actiepunt "Open State Foundation mailen" staat in CLAUDE.md. Het
privacypunt uit fase 2 hoort in die mail thuis.

---

## Fase 0: Stabiel onder belasting — afgerond (PR #1)
Doel: de app blijft werken als meerdere mensen hem tegelijk gebruiken, en na updates.

| # | Wat | Waar |
|---|---|---|
| 0.1 | **429 direct afhandelen**: geen lange retries meer. Globale pauze (`blokkade_tot`): tijdens een blokkade melden, zonder verzoek. `req_retry(max_seconds = 30)`, geen tweede poging na 429. | `R/api_ori.R` `post_json()` |
| 0.2 | **Live zoekvragen niet-blokkerend** via `shiny::ExtendedTask` + `mirai` (werkt met Shiny 1.14), met een busy-indicator. | `app.R` `doe_ophalen()` |
| 0.3 | **Data-tak controleren**: `SCHEMA_VERSIE` en `METHODE_VERSIE` in elk bestand en in de sleutel. Bij een afwijking, of data ouder dan 7 dagen: live ophalen. Ouder dan 2 dagen: waarschuwing in de status. Een echte 404 één uur onthouden, andere fouten 1 minuut. | `R/voorberekend.R`, `scripts/voorbereken.R` |
| 0.4 | **Jaarwisseling**: één constante `STANDAARD_BEGINJAAR`. Het jaar per sessie bepalen, niet bij het laden. Terugvallen op de laatste set uit `overzicht.csv`. | `R/config.R`, `R/voorberekend.R`, `app.R` |
| 0.5 | **Gemeentegrenzen en Iv3 één keer per proces** laden (globaal/memoise) in plaats van per sessie. Mislukte CBS-verzoeken kort onthouden. | `app.R` 312–316, 425–442 |
| 0.6 | **Invoer begrenzen**: maximaal 8 termen; woordvormen alleen voor termen van 4+ tekens. | `app.R`, `R/namen.R` |
| 0.7 | **CI**: `test.yml` draait testthat bij elke push/PR (zonder netwerk) en controleert dat `manifest.json` actueel is. | `.github/workflows/` |
| 0.8 | **Nachtelijke taak controleert vóór publiceren**: aantal gemeenten en totalen vergelijken met de vorige versie (±20%). Bestanden die niet in `overzicht.csv` staan opruimen. Rood bij gedeeltelijk falen. | `scripts/voorbereken.R`, `voorbereken.yml` |

Klaar als: meerdere gelijktijdige sessies in de test blijven bruikbaar tijdens een live
zoekvraag of een gesimuleerde 429; een oud databestand leidt tot live ophalen, niet tot
een crash; CI is groen.

## Fase 1: Cijfers die kloppen — afgerond (PR #2); 1.7 wacht op beoordeling van de steekproef
Doel: een ranking die je kunt verdedigen.

| # | Wat | Waar |
|---|---|---|
| 1.1 | **Samenvoegen van dubbelen doormeten en corrigeren.** Meet eerst het aandeel botsingen per archiefgrootte. Daarna: de noemer niet samenvoegen (of met een betere sleutel), en per gemeente samenvoegen, niet landelijk. README bijwerken. | `R/api_ori.R` `telling_aggs()`, `tel()` |
| 1.2 | **Jaren met dekking**: per gemeente alleen jaren met archief in de noemer van `per_100k` en de drempel. Het lopende jaar telt als fractie. Dekking (eerste jaar) tonen in tabel en profiel. | `R/api_ori.R` `haal_data_op()` |
| 1.3 | **0 treffers ≠ geen archief**: 0 wordt de laagste kleurklasse, "geen archief" blijft grijs met arcering. Rang "X van Y" over alle gemeenten met archief. | `R/kaart.R`, `app.R` |
| 1.4 | **Minimumaantal treffers en onzekerheid**: in de ranking een aanduiding bij minder dan ~10 treffers, optioneel een bandbreedte. | `app.R` ranking, `R/plots.R` |
| 1.5 | **Contextfilter gelijk voor alle velden** (titel, beschrijving en tekst), en `CULTUUR_REGEX` met woordgrenzen of een expliciete lijst (nu ontloopt "bedrijfscultuur" de contexteis). | `R/api_ori.R` `term_query()`, `R/config.R` |
| 1.6 | **Onbekende archieven en fusies signaleren**: de nachtelijke taak meldt archieven zonder CBS-koppeling. De `FUSIES`-lijst aanvullen (Dijk en Waard, Maashorst e.d. controleren). | `scripts/voorbereken.R`, `R/config.R` |
| 1.7 | **Validatie-steekproef**: script dat per term ~50 treffers met fragment naar CSV exporteert, met een kolom "relevant ja/nee". Na het invullen berekent het de precisie per term, die de README en de uitleg noemen. | nieuw `scripts/validatie.R` |

Klaar als: cijfers vóór en na naast elkaar gezet en de verschillen verklaard; precisie
per term bekend (na het invullen door een vakinhoudelijk persoon).

## Fase 2: Veilig en zorgvuldig
| # | Wat | Waar |
|---|---|---|
| 2.1 | **Fragmenten zelf escapen**: ES laat ongebruikelijke markeringstekens zetten, alles wordt ge-escaped, daarna `<mark>`. Niet meer vertrouwen op `encoder = "html"`. | `R/api_ori.R` `haal_fragmenten()`, `app.R` |
| 2.2 | **Privacy**: privacytekst met contactadres voor verwijderverzoeken; geen fragmenten tonen bij inspraak- of bezwaarstukken (herkenning op titel). | `app.R`, README |
| 2.3 | **Data-tak als JSON/Parquet** in plaats van RDS (geen uitvoerbare R-objecten van internet), of in elk geval de klassen controleren. Actions vastzetten op een commit-SHA. | `R/voorberekend.R`, workflows |
| 2.4 | **CSV-injectie**: titels die met `= + - @` beginnen escapen, en `veilige_link` ook in de documentenexport. | `R/export.R`, `app.R` |

## Fase 3: Begrijpelijk en toegankelijk (breed publiek)
| # | Wat | Waar |
|---|---|---|
| 3.1 | **Direct resultaat bij openen**: de standaardvraag laadt automatisch (voorberekend). De knop heet "Zoeken". `useBusyIndicators()`. | `app.R` |
| 3.2 | **Tabblad "Uitleg"**: hoe lees je dit, de methode in gewone taal, de precisie uit 1.7, kanttekeningen. | `app.R`, nieuw `R/uitleg.R` |
| 3.3 | **Verhaal per gemeente** in het profiel, bijvoorbeeld: "Tilburg bespreekt deze thema's vaker dan 80% van de gemeenten; het cultuurbudget ligt boven de mediaan." | `app.R` profiel |
| 3.4 | **Toegankelijkheid**: `lang = "nl"`, `alt` bij elke grafiek, budgetinformatie ook zonder hover (klik of tabel), NA-kleur duidelijk anders, het lopende jaar als open punt of stippellijn. | `app.R`, `R/plots.R`, `R/kaart.R` |
| 3.5 | **Kleine UX-bugs**: eigen termen blijven zichtbaar na een gedeelde link; de link bewaart het getoonde resultaat; de themakeuze reset; "€–" bij ontbrekend budget; dubbele trendberekening (`bindEvent`). | `app.R` |
| 3.6 | **Mobiele weergave** controleren en verbeteren (smalle zijbalk, kaarthoogte). | `app.R` CSS |

## Fase 4: Verdieping (sector en beleid)
| # | Wat |
|---|---|
| 4.1 | **Documentsoort** (motie, amendement, raadsvoorstel, besluit, bijlage, verslag) uit titelwoorden, als filter en als uitsplitsing. |
| 4.2 | **Budget per thema**: bibliotheek → taakveld 5.6, cultuureducatie → 5.3; afschrijvingen en verrekeningen apart. |
| 4.3 | **Vergelijkbare gemeenten**: vergelijken met gemeenten van dezelfde grootteklasse of stedelijkheid (CBS). |
| 4.4 | **Meer voorberekende perioden** (bijv. 2015–nu, laatste 3 jaar) voor snelle langjarige trends. |

## Fase 5: Onderhoud en portfolio
| # | Wat |
|---|---|
| 5.1 | **Iv3-tabellen automatisch vinden** via de catalogus van dataderden.cbs.nl; budgetkeuzes afleiden uit wat beschikbaar is; `IV3_JAAR` en de jaartallen in de README opruimen. |
| 5.2 | **Eén bron voor pakketten**: `renv.lock`, gebruikt door de Action, CI en het manifest; zoeksleutel als leesbare tekst in plaats van een hash van een R-object. |
| 5.3 | **app.R opsplitsen in Shiny-modules** per tabblad; helper `opties_uit(input)`. |
| 5.4 | **Geheugen**: verwerkte resultaten cachen in plaats van ruwe JSON (max ~50 MB); `bindCache()` op grafieken. |
| 5.5 | **End-to-end-test** met shinytest2 of chromote tegen de online app (dagelijks, ook als uptime-check). |
| 5.6 | **Screenshots en projectpagina automatisch bijwerken** na een release. |

---

## Werkwijze en verificatie
- **Per fase een eigen branch en pull request**, zodat CI (0.7) meekijkt; na akkoord mergen,
  waarna Connect Cloud automatisch publiceert.
- **Zuinig met de API** (CLAUDE.md): tests zonder netwerk met fixtures, en hooguit één
  handmatige live-run per fase.
- **Per fase**:
  - testthat;
  - een testServer-scenario voor de nieuwe functies;
  - een controle met chromote tegen de online app (klikken en de status uitlezen);
  - bij fase 1 een vergelijking van de cijfers vóór en na, met toelichting.
- **Na fase 0 en 1** worden de README en de uitleg bijgewerkt met de nieuwe methode.

## Voorgestelde volgorde
Fase 0 → 1 → 2 → 3 → 4 → 5. Fase 0 en 1 zijn nodig voordat je het dashboard breed
deelt. Fase 2 hoort bij de mail aan Open State. Daarna kun je 3–5 naar behoefte
kiezen.
