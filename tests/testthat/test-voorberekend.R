opt <- list(context = TRUE, woordvormen = FALSE, dedup = TRUE)

nep_resultaat <- function(termen = c("a", "b"), jaren = c(2020L, 2026L),
                          berekend_op = Sys.time(), schema = SCHEMA_VERSIE) {
  list(per_gemeente = tibble(key = "x", totaal = 1), jaren_nl = tibble(),
       docs = tibble(), totaal_docs = 42, termen = termen, jaren = jaren,
       opties = opt, inwoners_ok = TRUE, berekend_op = berekend_op,
       schema = schema)
}

# Zet een tijdelijke map neer als 'data-tak' en schrijf er bestanden in
lokale_datatak <- function(env = parent.frame()) {
  map <- withr::local_tempdir(.local_envir = env)
  withr::local_options(cultuur.voorberekend_url =
                         paste0("file:///", normalizePath(map, winslash = "/")),
                       .local_envir = env)
  rm(list = ls(voorberekend_geheugen), envir = voorberekend_geheugen)
  map
}
schrijf <- function(map, res, jaren = res$jaren) {
  saveRDS(res, file.path(map, sprintf("zoek_%s.rds",
                                      zoek_sleutel(res$termen, jaren, opt))))
}

test_that("zoek_sleutel hangt niet af van volgorde of getaltype", {
  a <- zoek_sleutel(c("b", "a"), c(2020, 2026), opt)
  expect_equal(a, zoek_sleutel(c("a", "b"), c(2020L, 2026L), opt))
  expect_false(a == zoek_sleutel(c("a", "b"), c(2021L, 2026L), opt))
  expect_false(a == zoek_sleutel(c("a", "b"), c(2020L, 2026L),
                                 modifyList(opt, list(dedup = FALSE))))
})

test_that("een ander aantal cultuurwoorden geeft een andere sleutel", {
  a <- zoek_sleutel("a", c(2020, 2026), opt)
  withr::local_envvar()  # geen effect; alleen voor de duidelijkheid
  oud <- CULTUURWOORDEN
  assign("CULTUURWOORDEN", c(oud, "circus"), envir = globalenv())
  on.exit(assign("CULTUURWOORDEN", oud, envir = globalenv()))
  expect_false(a == zoek_sleutel("a", c(2020, 2026), opt))
})

test_that("haal_resultaat gebruikt bruikbare voorberekende data", {
  map <- lokale_datatak()
  schrijf(map, nep_resultaat())
  res <- haal_resultaat(c("b", "a"), c(2020, 2026), opt)
  expect_equal(res$totaal_docs, 42)
  expect_equal(res$bron, "voorberekend")
  expect_null(lees_voorberekend("bestaat_niet.rds"))
})

test_that("oud schema, te oude data of een kapot bestand worden niet gebruikt", {
  expect_false(bruikbaar_resultaat(nep_resultaat(schema = 0L)))
  expect_false(bruikbaar_resultaat(
    nep_resultaat(berekend_op = Sys.time() - (MAX_LEEFTIJD_DAGEN + 1) * 86400)))
  expect_false(bruikbaar_resultaat(list(totaal_docs = 42)))
  expect_false(bruikbaar_resultaat("geen lijst"))
  expect_true(bruikbaar_resultaat(nep_resultaat()))

  map <- lokale_datatak()
  schrijf(map, nep_resultaat(schema = 0L))
  expect_null(zoek_voorberekend(c("a", "b"), c(2020L, 2026L), opt))
})

test_that("rond de jaarwisseling valt de standaardperiode terug op vorig jaar", {
  map <- lokale_datatak()
  vorig <- c(STANDAARD_BEGINJAAR, huidig_jaar() - 1L)
  schrijf(map, nep_resultaat(jaren = vorig))
  res <- zoek_voorberekend(c("a", "b"), standaard_periode(), opt)
  expect_equal(res$jaren, vorig)
  # Een andere periode valt niet terug
  expect_null(zoek_voorberekend(c("a", "b"), c(2015L, huidig_jaar()), opt))
})

test_that("zonder URL wordt er niets voorberekends gelezen", {
  withr::local_options(cultuur.voorberekend_url = NA)
  expect_null(lees_voorberekend("zoek_x.rds"))
})
