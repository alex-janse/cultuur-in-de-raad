# --- CBS: inwoners per gemeente ----------------------------------------------

# Verandert hooguit jaarlijks: één keer per R-sessie ophalen.
cbs_cache <- new.env()
haal_inwoners <- function() {
  if (!is.null(cbs_cache$inwoners)) return(cbs_cache$inwoners)
  meta <- cbs_get_meta(CBS_TABEL_BEVOLKING)
  perioden <- tail(grep("JJ00$", meta$Perioden$Key, value = TRUE), 2)
  d <- cbs_get_data(CBS_TABEL_BEVOLKING, Perioden = perioden,
                    RegioS = has_substring("GM"),
                    select = c("RegioS", "Perioden", "TotaleBevolking_1"))
  if (nrow(d) == 0) stop("CBS gaf geen bevolkingsdata terug.")

  inw <- d |>
    filter(!is.na(TotaleBevolking_1)) |>
    group_by(RegioS) |>
    slice_max(Perioden, n = 1) |>
    ungroup() |>
    left_join(meta$RegioS |> select(Key, cbs_naam = Title),
              by = c("RegioS" = "Key")) |>
    mutate(
      gemeentecode = trimws(RegioS),
      cbs_naam = trimws(cbs_naam),
      key = coalesce(unname(CBS_NAAM_NAAR_KEY[cbs_naam]), naar_key(cbs_naam)),
      cbs_naam = sub(" [(]gemeente[)]$", "", cbs_naam),
      inwoners = as.numeric(TotaleBevolking_1),
      inwoners_jaar = substr(Perioden, 1, 4)
    ) |>
    select(key, gemeentecode, cbs_naam, inwoners, inwoners_jaar)

  cbs_cache$inwoners <- inw
  inw
}

# --- CBS: gemeentelijke lasten voor cultuur (Iv3) -----------------------------

haal_cultuurlasten <- function(jaar = IV3_JAAR) {
  if (!is.null(cbs_cache$lasten)) return(cbs_cache$lasten)

  tabel <- IV3_TABELLEN[[as.character(jaar)]]
  basis <- sprintf("https://dataderden.cbs.nl/ODataFeed/odata/%s", tabel)
  # Sleutels zijn met spaties aangevuld tot 6 tekens: '5.3   '
  filter <- sprintf(
    "Verslagsoort eq '%sX005' and startswith(Categorie,'L') and (%s)", jaar,
    paste(sprintf("TaakveldBalanspost eq '%-6s'", IV3_TAAKVELDEN),
          collapse = " or "))

  req <- request(paste0(basis, "/TypedDataSet")) |>
    req_url_query(`$filter` = filter,
                  `$select` = "TaakveldBalanspost,Gemeenten,k_1ePlaatsing_1") |>
    req_headers(Accept = "application/json") |>  # anders Atom-XML
    req_timeout(90)

  rijen <- list()
  repeat {  # de feed pagineert per 10.000 rijen
    js <- req |> req_retry(max_tries = 3) |> req_perform() |>
      resp_body_json(simplifyVector = TRUE)
    if (is.null(js$value)) stop("Onverwachte JSON-structuur van CBS Iv3.")
    rijen[[length(rijen) + 1]] <- js$value
    volgende <- js[["odata.nextLink"]]
    if (is.null(volgende)) break
    req <- request(volgende) |> req_headers(Accept = "application/json")
  }

  inw_jaar <- min(jaar, HUIDIG_JAAR - 1)
  inwoners <- cbs_get_data(
    "03759ned", Perioden = paste0(inw_jaar, "JJ00"), Geslacht = "T001038",
    Leeftijd = "10000", BurgerlijkeStaat = "T001019",
    RegioS = has_substring("GM")
  ) |>
    transmute(gemeentecode = trimws(RegioS),
              inwoners_iv3 = as.numeric(BevolkingOp1Januari_1))

  lasten <- bind_rows(rijen) |>
    mutate(gemeentecode = trimws(Gemeenten)) |>
    group_by(gemeentecode) |>
    summarise(lasten_keur = sum(k_1ePlaatsing_1, na.rm = TRUE),
              .groups = "drop") |>
    left_join(inwoners, by = "gemeentecode") |>
    transmute(gemeentecode,
              # 0 betekent: (nog) niet aangeleverd
              cultuur_per_inw = ifelse(lasten_keur > 0,
                                       lasten_keur * 1000 / inwoners_iv3,
                                       NA_real_))

  cbs_cache$lasten <- lasten
  lasten
}
