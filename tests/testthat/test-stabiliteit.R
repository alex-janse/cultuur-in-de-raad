test_that("tijdens een blokkade doet post_json geen verzoek en meldt het direct", {
  zet_blokkade(120)
  on.exit(api_status$blokkade_tot <- NULL)
  # Een onbereikbare URL: als er tóch een verzoek ging, zou dat anders falen
  withr::local_options(list())
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
