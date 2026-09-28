rij <- function(documenten = 1000, gemeenten = 290, methode = "abc12345",
                periode = "2020-2026") {
  data.frame(set = "Standaard", documenten = documenten, gemeenten = gemeenten,
             methode = methode, periode = periode, sleutel = "s1")
}

test_that("lees_overzicht leest de methode-hash als tekst", {
  pad <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(rbind(rij(methode = "01234567"), rij(methode = "1234e567")),
                   pad, row.names = FALSE)
  d <- lees_overzicht(pad)
  expect_equal(d$methode, c("01234567", "1234e567"))
  expect_type(d$documenten, "integer")
  expect_equal(nrow(lees_overzicht(file.path(tempdir(), "bestaat_niet.csv"))), 0)
})

test_that("verdacht: grote afwijking, tenzij de vorige nacht hetzelfde gaf", {
  vorig <- rij()
  expect_null(verdacht(rij(documenten = 1100), vorig))
  expect_match(verdacht(rij(documenten = 1500), vorig), "1500 documenten")
  expect_match(verdacht(rij(gemeenten = 200), vorig), "200 archieven")
  # Andere methode of periode: niet te vergelijken, dus niet verdacht
  expect_null(verdacht(rij(documenten = 1500, methode = "anders"), vorig))
  expect_null(verdacht(rij(documenten = 1500), NULL))

  # Bevestigd door de afgekeurde uitkomst van de vorige nacht
  expect_null(verdacht(rij(documenten = 1500), vorig, rij(documenten = 1480)))
  # ...maar niet als die te veel verschilt
  expect_match(verdacht(rij(documenten = 1500), vorig, rij(documenten = 1300)),
               "documenten")
})

test_that("onplausibel keurt lege of halflege CBS-data af", {
  goed <- data.frame(gemeentecode = sprintf("GM%04d", 1:342), inwoners = 1:342)
  expect_null(onplausibel("inwoners", goed))
  expect_match(onplausibel("inwoners", goed[1:10, ]), "10 gemeenten")
  leeg <- data.frame(gemeentecode = goed$gemeentecode, cultuur_per_inw = NA_real_)
  expect_match(onplausibel("2026_begroting", leeg), "100% van cultuur_per_inw")
  expect_match(onplausibel("2026_begroting", goed), "ontbreekt")
  expect_null(onplausibel("grenzen", goed))
})

test_that("wachttijd wacht bij een blokkade tot die voorbij is", {
  withr::defer(api_status$blokkade_tot <- NULL)
  zet_blokkade(600)
  fout <- tryCatch(blokkade_fout(), error = \(e) e)
  expect_gt(wachttijd(fout), 600)
  expect_equal(wachttijd(simpleError("HTTP 429 Too Many Requests")), 300)
  expect_equal(wachttijd(simpleError("time-out")), 60)
})

test_that("op_te_ruimen: oude zoekresultaten en Iv3-keuzes", {
  bestanden <- c("zoek_a.rds", "zoek_b.rds", "iv3_2024_rekening.rds",
                 "iv3_2019_rekening.rds", "cbs_inwoners.rds", "overzicht.csv")
  expect_setequal(op_te_ruimen(bestanden, "a", keuzes = "2024_rekening"),
                  c("zoek_b.rds", "iv3_2019_rekening.rds"))
})

# --- werk_bij: bijwerken van CBS- en PDOK-onderdelen --------------------------

goede_data <- data.frame(gemeentecode = sprintf("GM%04d", 1:342), inwoners = 1:342)
versie <- \(v) \() list(versie = v, bron_datum = "2026-07-31")
kapot <- \() stop("CBS onbereikbaar")

run <- function(forceer = FALSE) {
  withr::local_options(cultuur.cache_map = withr::local_tempdir(.local_envir = parent.frame()),
                       .local_envir = parent.frame())
  nieuwe_run(withr::local_tempdir(.local_envir = parent.frame()), forceer = forceer,
             vandaag = "2026-09-28", slaap = \(s) NULL)
}

test_that("werk_bij: eerste keer ophalen, daarna alleen bij een nieuwe versie", {
  staat <- run()
  gehaald <- 0
  haal <- \(nieuw) { gehaald <<- gehaald + 1; goede_data }

  expect_true(werk_bij(staat, "inwoners", "cbs_inwoners.rds", versie("v1"), haal))
  expect_equal(staat$versies$versie, "v1")
  expect_false(werk_bij(staat, "inwoners", "cbs_inwoners.rds", versie("v1"), haal))
  expect_equal(gehaald, 1)
  expect_true(werk_bij(staat, "inwoners", "cbs_inwoners.rds", versie("v2"), haal))
  expect_equal(gehaald, 2)
  expect_equal(nrow(staat$versies), 1)
  expect_length(staat$mislukt, 0)
})

test_that("werk_bij: onbereikbare bron is een waarschuwing, forceren haalt toch op", {
  staat <- run()
  werk_bij(staat, "inwoners", "cbs_inwoners.rds", versie("v1"), \(n) goede_data)
  expect_false(werk_bij(staat, "inwoners", "cbs_inwoners.rds", kapot, \(n) stop("niet aanroepen")))
  expect_equal(staat$waarschuwingen, "inwoners")
  expect_length(staat$mislukt, 0)

  staat$forceer <- TRUE
  expect_true(werk_bij(staat, "inwoners", "cbs_inwoners.rds", kapot, \(n) goede_data))
})

test_that("werk_bij: onplausibele data vervangt de vorige versie niet", {
  staat <- run()
  werk_bij(staat, "inwoners", "cbs_inwoners.rds", versie("v1"), \(n) goede_data)
  expect_false(werk_bij(staat, "inwoners", "cbs_inwoners.rds", versie("v2"),
                        \(n) goede_data[1:5, ]))
  expect_match(staat$mislukt, "inwoners \\(5 gemeenten")
  expect_equal(nrow(readRDS(file.path(staat$uitvoer, "cbs_inwoners.rds"))), 342)
  # De versie blijft de oude, zodat de volgende nacht opnieuw probeert
  expect_equal(staat$versies$versie, "v1")
})

test_that("werk_bij geeft de gevonden versie door aan het ophalen", {
  staat <- run()
  gevraagd <- NULL
  werk_bij(staat, "grenzen", "gemeentegrenzen.rds",
           \() list(versie = "PDOK 2026", bron_datum = NA_character_, jaar = 2026L),
           \(nieuw) { gevraagd <<- nieuw$jaar; goede_data })
  expect_equal(gevraagd, 2026L)
})
