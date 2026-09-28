test_that("tijdens een blokkade doet post_json geen verzoek en meldt het direct", {
  zet_blokkade(120)
  on.exit(api_status$blokkade_tot <- NULL)
  # Gaat er tóch een verzoek uit, dan faalt de test zonder de echte API te raken
  httr2::local_mocked_responses(function(req) stop("verzoek tijdens blokkade"))
  fout <- tryCatch(post_json(list(size = 0, uniek = runif(1))), error = identity)
  expect_s3_class(fout, "api_blokkade")
  expect_match(conditionMessage(fout), "429")
  expect_match(conditionMessage(fout), "2 minuten")
})

test_that("een verlopen blokkade telt niet meer", {
  api_status$blokkade_tot <- Sys.time() - 1
  on.exit(api_status$blokkade_tot <- NULL)
  expect_null(blokkade_tot())
})

test_that("per_proces laadt één keer en onthoudt een fout even", {
  naam <- paste0("test_", runif(1))
  teller <- 0
  maak <- function() { teller <<- teller + 1; "data" }
  expect_equal(per_proces(naam, maak), "data")
  expect_equal(per_proces(naam, maak), "data")
  expect_equal(teller, 1)

  fout_naam <- paste0("fout_", runif(1))
  pogingen <- 0
  faal <- function() { pogingen <<- pogingen + 1; stop("CBS plat") }
  expect_error(per_proces(fout_naam, faal), "CBS plat")
  expect_error(per_proces(fout_naam, faal), "CBS plat")
  expect_equal(pogingen, 1)  # tweede keer uit het geheugen, geen nieuwe poging
})

test_that("hooguit MAX_TERMEN zoektermen", {
  expect_length(schoon_termen(paste0("term", 1:20)), MAX_TERMEN)
})

test_that("woordvormen alleen vanaf MIN_TEKENS_WOORDVORMEN tekens", {
  opt <- list(context = FALSE, woordvormen = TRUE)
  expect_named(term_query("kun", opt), "multi_match")
  expect_named(term_query("kunstenplan", opt), "query_string")
})

test_that("het jaar en de standaardperiode worden per aanroep bepaald", {
  expect_equal(standaard_periode(), c(STANDAARD_BEGINJAAR, huidig_jaar()))
  expect_type(standaard_periode(), "integer")
})

test_that("per_proces laadt na afloop van de geldigheid opnieuw", {
  naam <- paste0("verloop_", runif(1))
  teller <- 0
  maak <- function() { teller <<- teller + 1; teller }
  expect_equal(per_proces(naam, maak, geldig = 3600), 1)
  proces_geheugen[[naam]]$geldig_tot <- Sys.time() - 1
  expect_equal(per_proces(naam, maak, geldig = 3600), 2)
})

test_that("schijf_cache negeert een kapot bestand en schrijft het opnieuw", {
  withr::local_options(cultuur.cache_map = withr::local_tempdir(),
                       cultuur.voorberekend_url = NA)
  writeLines("geen rds", file.path(cache_map(), "kapot.rds"))
  expect_equal(schijf_cache("kapot", 30, \() data.frame(x = 1)), data.frame(x = 1))
  expect_equal(readRDS(file.path(cache_map(), "kapot.rds")), data.frame(x = 1))
  expect_length(list.files(cache_map(), "[.]tmp$"), 0)
})

test_that("schijf_cache geeft de voorberekende versie voorrang op de schijfcache", {
  map <- withr::local_tempdir()
  withr::local_options(cultuur.cache_map = withr::local_tempdir(),
                       cultuur.voorberekend_url = paste0("file:///", normalizePath(map, winslash = "/")))
  rm(list = ls(voorberekend_geheugen), envir = voorberekend_geheugen)
  saveRDS(data.frame(x = "oud"), file.path(cache_map(), "tabel.rds"))
  saveRDS(data.frame(x = "nieuw"), file.path(map, "tabel.rds"))
  expect_equal(schijf_cache("tabel", 30, \() stop("niet live"))$x, "nieuw")
})

test_that("een afgebroken of afgekapt antwoord geldt als onvolledig", {
  for (json in list(
    list(timed_out = TRUE, `_shards` = list(failed = 0)),
    list(`_shards` = list(failed = 0),
         aggregations = list(gemeenten = list(sum_other_doc_count = 12)))
  )) {
    httr2::local_mocked_responses(function(req) httr2::response_json(body = json))
    expect_warning(post_json(list(size = 0, uniek = runif(1))), "onvolledig")
  }
})
