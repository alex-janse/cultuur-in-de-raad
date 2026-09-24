# --- Configuratie ------------------------------------------------------------

# Publieke Elasticsearch-endpoint achter OpenBesluitvorming.nl.
# Elke gemeente heeft een eigen index ("ori_<gemeente>_<timestamp>"); die index
# is daarmee de 'source' waarop we aggregeren.
API_BASIS <- "https://api.openraadsinformatie.nl/v1/elastic"
MAX_DOCS <- 100          # aantal voorbeelddocumenten (limit=100)
MIN_DOCS_PER_JAAR <- 400 # kleinere archieven geven onbetrouwbare relatieve cijfers
MIN_INWONERS <- 20000    # zeer kleine gemeenten domineren anders 'per inwoner'
EERSTE_JAAR <- 2010
HUIDIG_JAAR <- as.integer(format(Sys.Date(), "%Y"))

CBS_TABEL_BEVOLKING <- "70072ned"  # Regionale kerncijfers Nederland

TERMEN <- list(
  "Kernthema's" = c("amateurkunst", "cultuurbeoefening", "cultuureducatie",
                    "kunstenplan", "bibliotheek"),
  "Aanverwante termen" = c("talentontwikkeling", "cultuurparticipatie",
                           "muziekonderwijs", "muziekschool", "podiumkunsten",
                           "cultuurbeleid", "erfgoed")
)
STANDAARD_TERMEN <- c("amateurkunst", "cultuurbeoefening", "talentontwikkeling")

MAATSTAVEN <- c("Per 1.000 raadsdocumenten" = "relatief",
                "Per 100.000 inwoners (per jaar)" = "inwoners",
                "Absoluut (aantal documenten)" = "absoluut")

# Hardcoded coördinaten; 'key' = genormaliseerde gemeentenaam (zie naar_key()).
GEMEENTEN <- tribble(
  ~key,             ~gemeente,          ~lat,    ~lon,
  "amsterdam",      "Amsterdam",        52.3676, 4.9041,
  "rotterdam",      "Rotterdam",        51.9244, 4.4777,
  "den_haag",       "Den Haag",         52.0705, 4.3007,
  "utrecht",        "Utrecht",          52.0907, 5.1214,
  "eindhoven",      "Eindhoven",        51.4416, 5.4697,
  "groningen",      "Groningen",        53.2194, 6.5665,
  "tilburg",        "Tilburg",          51.5555, 5.0913,
  "almere",         "Almere",           52.3508, 5.2647,
  "breda",          "Breda",            51.5719, 4.7683,
  "nijmegen",       "Nijmegen",         51.8126, 5.8372,
  "apeldoorn",      "Apeldoorn",        52.2112, 5.9699,
  "arnhem",         "Arnhem",           51.9851, 5.8987,
  "haarlem",        "Haarlem",          52.3874, 4.6462,
  "haarlemmermeer", "Haarlemmermeer",   52.3030, 4.6890,
  "amersfoort",     "Amersfoort",       52.1561, 5.3878,
  "zaanstad",       "Zaanstad",         52.4570, 4.7510,
  "enschede",       "Enschede",         52.2215, 6.8937,
  "den_bosch",      "'s-Hertogenbosch", 51.6978, 5.3037,
  "zwolle",         "Zwolle",           52.5168, 6.0830,
  "zoetermeer",     "Zoetermeer",       52.0575, 4.4931,
  "leiden",         "Leiden",           52.1601, 4.4970,
  "maastricht",     "Maastricht",       50.8514, 5.6910,
  "dordrecht",      "Dordrecht",        51.8133, 4.6901,
  "ede",            "Ede",              52.0402, 5.6649,
  "alkmaar",        "Alkmaar",          52.6324, 4.7534,
  "emmen",          "Emmen",            52.7792, 6.9069,
  "westland",       "Westland",         51.9990, 4.2090,
  "delft",          "Delft",            52.0116, 4.3571,
  "venlo",          "Venlo",            51.3704, 6.1724,
  "deventer",       "Deventer",         52.2661, 6.1552,
  "leeuwarden",     "Leeuwarden",       53.2012, 5.7999,
  "lelystad",       "Lelystad",         52.5185, 5.4714,
  "helmond",        "Helmond",          51.4793, 5.6570,
  "hilversum",      "Hilversum",        52.2292, 5.1669,
  "heerlen",        "Heerlen",          50.8882, 5.9795,
  "amstelveen",     "Amstelveen",       52.3114, 4.8701,
  "gouda",          "Gouda",            52.0115, 4.7105,
  "assen",          "Assen",            52.9925, 6.5649,
  "purmerend",      "Purmerend",        52.5050, 4.9597,
  "sittard_geleen", "Sittard-Geleen",   51.0000, 5.8686,
  "oss",            "Oss",              51.7650, 5.5180,
  "roosendaal",     "Roosendaal",       51.5308, 4.4653,
  "noordoostpolder","Noordoostpolder",  52.7100, 5.7500,
  "hoogeveen",      "Hoogeveen",        52.7225, 6.4764
)

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

# --- CBS: gemeentelijke lasten voor cultuur (Iv3) -----------------------------

# "Gemeenten <jaar> onbewerkte Iv3-data" staan niet in de StatLine-catalogus
# maar op dataderden.cbs.nl. Bedragen in 1.000 euro ("1e plaatsing").
IV3_TABELLEN <- c(`2023` = "45063NED", `2024` = "45067NED",
                  `2025` = "45071NED", `2026` = "45078NED")
IV3_JAAR <- 2024                                  # meest recente jaarrekening
IV3_TAAKVELDEN <- c("5.3", "5.4", "5.5", "5.6")   # cultuur, musea, erfgoed, media/bibliotheek
