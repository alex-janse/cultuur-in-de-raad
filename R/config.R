# --- Configuratie ------------------------------------------------------------

# Publieke Elasticsearch-endpoint achter OpenBesluitvorming.nl.
# Elke gemeente heeft een eigen index ("ori_<gemeente>_<timestamp>"); die index
# is daarmee de 'source' waarop we aggregeren.
API_BASIS <- "https://api.openraadsinformatie.nl/v1/elastic"
MAX_DOCS <- 100          # aantal voorbeelddocumenten (limit=100)
MIN_DOCS_PER_JAAR <- 400 # kleinere archieven geven onbetrouwbare relatieve cijfers
MIN_INWONERS <- 20000    # zeer kleine gemeenten domineren anders 'per inwoner'
EERSTE_JAAR <- 2010
# Het jaar per aanroep bepalen, niet bij het laden: een proces kan over
# 1 januari heen blijven draaien.
huidig_jaar <- function() as.integer(format(Sys.Date(), "%Y"))

# Standaardperiode van de app én van de nachtelijke voorberekening; op één
# plek, zodat die twee niet uit elkaar kunnen lopen.
STANDAARD_BEGINJAAR <- 2020L
standaard_periode <- function() c(STANDAARD_BEGINJAAR, huidig_jaar())

# Versies van de voorberekende data. SCHEMA_VERSIE ophogen bij een andere
# structuur van het resultaat; de methode-versie volgt automatisch uit de
# instellingen die de telling bepalen (zie methode_versie() onderaan).
SCHEMA_VERSIE <- 1L
# Voorberekende data ouder dan dit wordt niet meer gebruikt (dan live)
MAX_LEEFTIJD_DAGEN <- 7
WAARSCHUW_LEEFTIJD_DAGEN <- 2

MAX_TERMEN <- 8               # meer termen = zwaardere zoekvraag voor de API
MIN_TEKENS_WOORDVORMEN <- 4   # 'ku*' is duur en loopt tegen limieten aan

CBS_TABEL_BEVOLKING <- "70072ned"  # Regionale kerncijfers Nederland

TERMEN <- list(
  "Kernthema's" = c("amateurkunst", "cultuurbeoefening", "cultuureducatie",
                    "kunstenplan", "bibliotheek"),
  "Aanverwante termen" = c("talentontwikkeling", "cultuurparticipatie",
                           "muziekonderwijs", "muziekschool", "podiumkunsten",
                           "cultuurbeleid", "erfgoed")
)
STANDAARD_TERMEN <- c("amateurkunst", "cultuurbeoefening", "talentontwikkeling")

# Kiezen in de dropdown 'Thema' vervangt de zoektermen door deze set
THEMASETS <- list(
  "Cultuurparticipatie" = c("amateurkunst", "cultuurbeoefening",
                            "cultuurparticipatie"),
  "Cultuuronderwijs" = c("cultuureducatie", "muziekonderwijs", "muziekschool"),
  "Talent & podium" = c("talentontwikkeling", "podiumkunsten"),
  "Bibliotheek & erfgoed" = c("bibliotheek", "erfgoed"),
  "Cultuurbeleid" = c("cultuurbeleid", "kunstenplan")
)

# Cultuurcontext: een term telt alleen als er binnen CONTEXT_AFSTAND woorden
# een van deze woorden staat. Zonder deze eis gaat bv. 'talentontwikkeling'
# grotendeels over sport en onderwijs (22.505 -> 6.967 treffers sinds 2020).
# Vaste woorden i.p.v. prefixen: 'kunst*' pakt ook kunstgras en kunstwerken
# (bruggen), en 'cultu*' loopt tegen de expansielimiet van de server aan.
CONTEXT_AFSTAND <- 15
CULTUURWOORDEN <- c(
  "cultuur", "culturele", "cultureel", "kunst", "kunsten", "kunstenaar",
  "kunstenaars", "kunstzinnig", "kunstzinnige", "muziek", "muziekles",
  "muzieklessen", "muziekschool", "muziekonderwijs", "theater", "theaters",
  "museum", "musea", "dans", "podium", "podiumkunsten", "erfgoed",
  "amateurkunst", "cultuureducatie", "cultuurparticipatie", "cultuurcoach",
  "cultuurcoaches", "creatief", "creatieve"
)
# Termen die zelf al over cultuur gaan krijgen geen extra contexteis
CULTUUR_REGEX <- "cultu|kunst|muziek|theater|muse|podium|erfgoed|dans"

MAATSTAVEN <- c("Per 1.000 raadsdocumenten" = "relatief",
                "Per 100.000 inwoners (per jaar)" = "inwoners",
                "Absoluut (aantal documenten)" = "absoluut")

# CBS-namen die niet automatisch op de ORI-indexnaam passen
CBS_NAAM_NAAR_KEY <- c(
  "Bergen (NH.)" = "bergen_nh",
  "Bergen (L.)" = "bergen",
  "'s-Gravenhage (gemeente)" = "den_haag",
  "'s-Hertogenbosch" = "den_bosch",
  "Capelle aan den IJssel" = "capelle_ad_ijssel",
  "Krimpen aan den IJssel" = "krimpen_ad_ijssel",
  "Bodegraven-Reeuwijk" = "bodegravenreeuwijk",
  "Hof van Twente" = "hofvantwente",
  "Kaag en Braassem" = "kaag_en_brasssem",
  "Nuenen, Gerwen en Nederwetten" = "nuenen"
)

# Bekendere namen voor weergave (CBS gebruikt de officiële naam)
WEERGAVENAAM <- c(den_haag = "Den Haag")

# Archieven van opgeheven gemeenten tellen mee bij de huidige gemeente.
# Let op: hun archief loopt maar tot de fusiedatum.
FUSIES <- c(
  weesp = "amsterdam",                       # 2022
  beemster = "purmerend",                    # 2022
  boxmeer = "land_van_cuijk", cuijk = "land_van_cuijk", grave = "land_van_cuijk",
  mill_en_st_hubert = "land_van_cuijk", sint_anthonis = "land_van_cuijk",  # 2022
  brielle = "voorne_aan_zee", westvoorne = "voorne_aan_zee",               # 2023
  binnenmaas = "hoeksche_waard"              # 2019
)

# --- CBS: gemeentelijke lasten voor cultuur (Iv3) -----------------------------

# "Gemeenten <jaar> onbewerkte Iv3-data" staan niet in de StatLine-catalogus
# maar op dataderden.cbs.nl. Bedragen in 1.000 euro ("1e plaatsing").
IV3_TABELLEN <- c(`2023` = "45063NED", `2024` = "45067NED",
                  `2025` = "45071NED", `2026` = "45078NED")
IV3_JAAR <- 2024                                  # meest recente jaarrekening
# Verslagsoort: X005 = jaarrekening, X000 = (primitieve) begroting
IV3_KEUZES <- c("Jaarrekening 2024" = "2024_rekening",
                "Jaarrekening 2023" = "2023_rekening",
                "Begroting 2026" = "2026_begroting",
                "Begroting 2025" = "2025_begroting")
IV3_TAAKVELDEN <- c("5.3", "5.4", "5.5", "5.6")   # cultuur, musea, erfgoed, media/bibliotheek

# Korte vingerafdruk van alles wat de telling bepaalt. Verandert er iets
# (bv. een cultuurwoord erbij), dan horen voorberekende resultaten niet meer
# bij de app en worden ze niet gebruikt.
methode_versie <- function() {
  substr(rlang::hash(list(
    CULTUURWOORDEN, CONTEXT_AFSTAND, CULTUUR_REGEX, MIN_DOCS_PER_JAAR,
    MIN_INWONERS, FUSIES, CBS_NAAM_NAAR_KEY, MIN_TEKENS_WOORDVORMEN
  )), 1, 8)
}
