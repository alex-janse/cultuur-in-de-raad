# --- Hulpfuncties: namen -----------------------------------------------------

# "Súdwest-Fryslân" -> "sudwest_fryslan", "Utrecht (gemeente)" -> "utrecht"
naar_key <- function(x) {
  x <- tolower(sub(" [(][^)]*[)]$", "", x))
  x <- iconv(x, "UTF-8", "ASCII//TRANSLIT")
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

# "ori_midden-delfland_20250525191105" -> "midden-delfland"
index_naar_ruw <- function(index) sub("_[0-9]{8,}$", "", sub("^ori_", "", index))

# Ruwe indexnaam -> key. Amsterdamse stadsdelen worden bij Amsterdam opgeteld,
# opgeheven gemeenten bij hun opvolger (zie FUSIES).
ruw_naar_key <- function(ruw) {
  key <- sub("^amsterdam_.*$", "amsterdam", naar_key(ruw))
  ifelse(key %in% names(FUSIES), unname(FUSIES[key]), key)
}

# Indexpatroon voor één gemeente ("_2*" voorkomt dat bergen ook bergen_nh pakt)
index_patroon <- function(ruwe_namen) {
  pat <- ifelse(grepl("^amsterdam", ruwe_namen), "ori_amsterdam*",
                paste0("ori_", ruwe_namen, "_2*"))
  paste(unique(pat), collapse = ",")
}

nette_naam <- function(key) {
  x <- gsub("_", " ", key)
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

fmt <- function(x, digits = 1) {
  ifelse(is.na(x), "–", formatC(x, format = "f", digits = digits,
                                big.mark = ".", decimal.mark = ","))
}

# Zoektermen van de gebruiker: kleine letters, alleen letters, cijfers,
# spaties, koppel- en apostroftekens; minimaal 2 tekens, geen dubbelen.
schoon_termen <- function(termen) {
  termen <- gsub("[^[:alnum:] '-]", "", tolower(trimws(termen)))
  termen <- unique(gsub("\\s+", " ", trimws(termen)))
  termen[nchar(termen) >= 2]
}
