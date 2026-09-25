# Cultuur in de raad

Hoe vaak praten Nederlandse gemeenteraden over amateurkunst, cultuureducatie,
cultuurbeoefening en verwante onderwerpen? En hoe verhoudt die aandacht zich
tot wat een gemeente aan cultuur uitgeeft?

Dit is een R Shiny-dashboard dat **live** de raadsdocumenten van ongeveer 290
gemeenten doorzoekt via [OpenBesluitvorming.nl](https://openbesluitvorming.nl),
en die combineert met open data van het CBS en PDOK. Alle bronnen zijn gratis
en hebben geen API-sleutel nodig.

**[Open het dashboard](https://01a0d57f-3a32-a4ab-9874-1761a480a727.share.connect.posit.cloud/)** ·
[Projectpagina](https://alex-janse.github.io/cultuur-in-de-raad/)

De app staat op Posit Connect Cloud (gratis). Bij de eerste bezoeker na een
stille periode moet hij even opstarten.

![Kaart en ranking](docs/img/kaart.png)

## Wat kun je ermee?

- **Zoeken op thema's of eigen termen**: kies een themaset (zoals
  *Cultuurparticipatie*) of typ zelf een term of woordgroep.
- **Kaart en ranking**: alle gemeenten ingekleurd naar aandacht. Je kiest de
  maatstaf:
  - per 1.000 raadsdocumenten, wat corrigeert voor de omvang van het archief;
  - per 100.000 inwoners;
  - absoluut.
- **Trend**: aandacht per jaar, met een eigen grafiek per term. Vergelijk tot
  vier gemeenten met heel Nederland.
- **Gemeenteprofiel**: klik op een gemeente voor:
  - de kerncijfers;
  - de trend naast die van Nederland;
  - de cultuurlasten per inwoner van 2023 tot en met 2026;
  - de nieuwste vermeldingen, met de zin waarin de term staat.
- **Aandacht vs. budget**: welke gemeenten praten veel over cultuur maar geven
  er weinig aan uit, en omgekeerd?
- **Export en delen**: tabellen als CSV (Excel-vriendelijk), grafieken als PNG,
  en een link die je zoekopdracht bewaart.

| Trend | Gemeenteprofiel |
|---|---|
| ![Trend](docs/img/trend.png) | ![Gemeenteprofiel](docs/img/profiel.png) |

![Aandacht versus budget](docs/img/budget.png)

## Starten

Je hebt R 4.4 of nieuwer nodig. Installeer eenmalig de pakketten:

```r
install.packages(c("shiny", "httr2", "jsonlite", "dplyr", "tidyr", "leaflet",
                   "ggplot2", "ggrepel", "cbsodataR", "sf", "cachem"))
```

Start daarna de app vanuit deze map:

```r
shiny::runApp()
```

Klik op **Haal Live Data Op**.

### Snel laden dankzij een nachtelijke berekening

Elke nacht rekent een GitHub Action
([`voorbereken.yml`](.github/workflows/voorbereken.yml)) de volgende zoekvragen
vooraf uit:

- de standaardtermen;
- elke themaset, met de standaardperiode en de standaardinstellingen;
- de CBS-cijfers en de gemeentegrenzen.

De uitkomsten komen op de tak [`data`](../../tree/data) van deze repository.
De app leest die eerst. Zo'n zoekvraag laadt daardoor direct, en de API van
OpenBesluitvorming wordt minder belast.

Alleen andere termen, perioden of instellingen gaan live naar de API. Dat
duurt ongeveer 15 seconden. Daarna blijft het resultaat een uur in het
geheugen.

De berekening kun je ook zelf starten, via het tabblad *Actions* op GitHub of
lokaal:

```r
source("scripts/voorbereken.R")   # of: Rscript scripts/voorbereken.R uitvoer
```

Tests draaien:

```r
testthat::test_dir("tests/testthat")
```

## Hoe wordt er geteld?

- **Telling**: het aantal raadsdocumenten (stukken, bijlagen, agendapunten)
  waarin een term voorkomt, niet het aantal keer dat de term genoemd wordt.
  De term wordt gezocht als exacte woordgroep in de titel, de beschrijving en
  de volledige tekst.
- **Dubbele bijlagen samenvoegen** (standaard aan): dezelfde bijlage hangt
  vaak bij meerdere agendapunten. Bestanden met precies dezelfde grootte
  tellen één keer. Dat scheelt ongeveer 20% van de treffers. De naam is geen
  goede sleutel, want veel bestanden heten "Bijlage 1.pdf".
- **Alleen in cultuurcontext** (standaard aan): een term die zelf niet over
  cultuur gaat (zoals *talentontwikkeling*) telt alleen als binnen 15 woorden
  een cultuurwoord staat (cultuur, kunst, muziek, theater, museum, erfgoed, …).
  Zonder deze eis gaat twee derde van de treffers voor *talentontwikkeling*
  over sport, onderwijs of jeugd.
- **Woordvormen** (optioneel): *amateurkunst* vindt dan ook
  *amateurkunstenaars*.
- **Per 1.000 raadsdocumenten**: de noemer is het aantal documenten van die
  gemeente in dezelfde periode. Gemeenten met minder dan 400 documenten per
  jaar tellen bij deze maatstaf niet mee.
- **Per 100.000 inwoners**: alleen voor gemeenten vanaf 20.000 inwoners.
  Kleinere gemeenten produceren ongeveer evenveel raadsstukken en zouden
  anders bovenaan staan.
- **Budget**: de gemeentelijke lasten per inwoner voor de taakvelden 5.3
  cultuur, 5.4 musea, 5.5 erfgoed en 5.6 media/bibliotheek. Bron: de
  Iv3-gemeentefinanciën van het CBS (jaarrekening 2023/2024, begroting
  2025/2026). Kapitaallasten en verrekeningen tellen mee. Het bedrag is dus
  een indicatie.

## Kanttekeningen

- **Niet alle gemeenten doen mee:** 51 van de 342 gemeenten hebben geen
  archief in OpenBesluitvorming, en die zijn grijs op de kaart.
- **Archieven gaan niet even ver terug:** vergelijk daarom liefst binnen één
  periode, bijvoorbeeld de laatste vijf jaar.
- **Fusiegemeenten:** de archieven van opgeheven gemeenten (Weesp, Beemster,
  Cuijk, Boxmeer, Brielle, …) tellen mee bij de huidige gemeente, maar lopen
  maar tot de fusiedatum.
- **Limiet op verzoeken:** de API van OpenBesluitvorming heeft een limiet.
  Bij veel zoekvragen kort na elkaar krijg je een melding (HTTP 429). Probeer
  het dan een paar minuten later opnieuw.
- **Aandacht is geen beleid:** een vermelding kan in een besluit staan, maar
  ook in een bijlage of een motie die is verworpen. Het gemeenteprofiel toont
  de zinnen, zodat je dat kunt nagaan.

## Projectstructuur

```
app.R              UI en server
R/config.R         termen, themasets, cultuurwoorden, fusies, CBS-tabellen
R/api_ori.R        zoekvragen aan OpenBesluitvorming (Elasticsearch)
R/cbs.R            inwoners en cultuurlasten (CBS)
R/geo.R            gemeentegrenzen (PDOK)
R/kaart.R          kaartopbouw
R/plots.R          grafieken
R/cache.R          schijf- en geheugencache
R/voorberekend.R   nachtelijk voorberekende data lezen
scripts/           nachtelijke voorberekening (GitHub Action)
R/export.R         CSV-export
R/namen.R          naamnormalisatie en opmaak
tests/testthat/    tests
docs/              projectpagina (GitHub Pages) en screenshots
```

## Bronnen

- **Raadsdocumenten**: [OpenBesluitvorming.nl](https://openbesluitvorming.nl) /
  Open Raadsinformatie van de [Open State Foundation](https://openstate.eu).
- **Inwoners en gemeentefinanciën**: [CBS](https://www.cbs.nl), open data
  (CC BY 4.0). Tabellen 70072ned, 03759ned en de Iv3-tabellen voor gemeenten.
- **Gemeentegrenzen en achtergrondkaart**: [PDOK](https://www.pdok.nl) /
  CBS en Kadaster.

## Licentie

De code valt onder de MIT-licentie, zie [LICENSE](LICENSE). Voor de data
gelden de voorwaarden van de bronnen hierboven.
