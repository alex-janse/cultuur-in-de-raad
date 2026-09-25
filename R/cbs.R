# --- CBS: inwoners per gemeente ----------------------------------------------

# Verandert hooguit jaarlijks: 30 dagen op schijf bewaren.
haal_inwoners <- function() {
  schijf_cache("cbs_inwoners", max_dagen = 30, function() {
    meta <- cbs_get_meta(CBS_TABEL_BEVOLKING)
    perioden <- tail(grep("JJ00$", meta$Perioden$Key, value = TRUE), 2)
    d <- cbs_get_data(CBS_TABEL_BEVOLKING, Perioden = perioden,
                      RegioS = has_substring("GM"),
                      select = c("RegioS", "Perioden", "TotaleBevolking_1"))
    if (nrow(d) == 0) stop("CBS gaf geen bevolkingsdata terug.")

    d |>
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
        cbs_naam = coalesce(unname(WEERGAVENAAM[key]),
                            sub(" [(]gemeente[)]$", "", cbs_naam)),
        inwoners = as.numeric(TotaleBevolking_1),
        inwoners_jaar = substr(Perioden, 1, 4)
      ) |>
      select(key, gemeentecode, cbs_naam, inwoners, inwoners_jaar)
  })
}

# --- CBS: gemeentelijke lasten voor cultuur (Iv3) -----------------------------

# keuze: "2024_rekening" of "2026_begroting" (zie IV3_KEUZES)
haal_cultuurlasten <- function(keuze = "2024_rekening") {
  delen <- strsplit(keuze, "_")[[1]]
  jaar <- as.integer(delen[1])
  verslagsoort <- if (delen[2] == "rekening") "X005" else "X000"

  schijf_cache(paste0("iv3_", keuze), max_dagen = 30, function() {
    tabel <- IV3_TABELLEN[[as.character(jaar)]]
    basis <- sprintf("https://dataderden.cbs.nl/ODataFeed/odata/%s", tabel)
    # Sleutels zijn met spaties aangevuld tot 6 tekens: '5.3   '
    filter <- sprintf(
      "Verslagsoort eq '%s%s' and startswith(Categorie,'L') and (%s)",
      jaar, verslagsoort,
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
    if (nrow(bind_rows(rijen)) == 0) {
      stop(sprintf("CBS heeft (nog) geen Iv3-data voor %s.", keuze))
    }

    inw_jaar <- min(jaar, huidig_jaar() - 1)
    inwoners <- cbs_get_data(
      "03759ned", Perioden = paste0(inw_jaar, "JJ00"), Geslacht = "T001038",
      Leeftijd = "10000", BurgerlijkeStaat = "T001019",
      RegioS = has_substring("GM")
    ) |>
      transmute(gemeentecode = trimws(RegioS),
                inwoners_iv3 = as.numeric(BevolkingOp1Januari_1))

    bind_rows(rijen) |>
      mutate(gemeentecode = trimws(Gemeenten)) |>
      group_by(gemeentecode) |>
      summarise(lasten_keur = sum(k_1ePlaatsing_1, na.rm = TRUE),
                .groups = "drop") |>
      left_join(inwoners, by = "gemeentecode") |>
      transmute(gemeentecode, keuze = keuze, jaar = jaar,
                # 0 betekent: (nog) niet aangeleverd
                cultuur_per_inw = ifelse(lasten_keur > 0,
                                         lasten_keur * 1000 / inwoners_iv3,
                                         NA_real_))
  })
}
