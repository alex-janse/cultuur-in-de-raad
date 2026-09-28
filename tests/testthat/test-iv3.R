# Budgetjaren (Iv3) automatisch vinden in de CBS-catalogus

catalogus <- c(`2024` = "T24", `2025` = "T25", `2026` = "T26", `2023` = "T23")

test_that("ontdek_iv3_keuzes kiest per soort de nieuwste jaren met genoeg cijfers", {
  # Jaarrekening 2026 bestaat nog niet (0 gemeenten), de rest wel
  met_data <- function(tabel, jaar, soort) {
    if (soort == "rekening" && jaar == 2026) 0L else 330L
  }
  k <- ontdek_iv3_keuzes(2, catalogus = catalogus, met_data = met_data)
  expect_equal(k$keuze, c("2025_rekening", "2024_rekening",
                          "2026_begroting", "2025_begroting"))
  expect_equal(k$label[1], "Jaarrekening 2025")
  expect_equal(k$tabel, c("T25", "T24", "T26", "T25"))
})

test_that("een jaar met cijfers van te weinig gemeenten wordt overgeslagen", {
  met_data <- function(tabel, jaar, soort) {
    if (jaar == 2025) IV3_MIN_GEMEENTEN - 1L else 330L
  }
  k <- ontdek_iv3_keuzes(2, catalogus = catalogus, met_data = met_data)
  expect_equal(k$keuze, c("2026_rekening", "2024_rekening",
                          "2026_begroting", "2024_begroting"))
  expect_error(ontdek_iv3_keuzes(2, catalogus = catalogus, met_data = \(...) 0L),
               "Geen Iv3-jaren")
})

test_that("iv3_catalogus leest alleen de gemeentetabellen", {
  httr2::local_mocked_responses(function(req) httr2::response_json(body = list(value = list(
    list(Identifier = "45078NED", Title = "Gemeenten 2026 onbewerkte Iv3-data"),
    list(Identifier = "45050NED", Title = "Gemeenten 2020 onbewerkte Iv3-data "),
    list(Identifier = "99999NED", Title = "Provincies 2026 onbewerkte Iv3-data")))))
  expect_equal(iv3_catalogus(), c(`2026` = "45078NED", `2020` = "45050NED"))
})

test_that("iv3_gemeenten_met_data telt alleen gemeenten met een bedrag", {
  httr2::local_mocked_responses(function(req) httr2::response_json(body = list(value = list(
    list(Gemeenten = "GM1", k_1ePlaatsing_1 = 10),
    list(Gemeenten = "GM1", k_1ePlaatsing_1 = 5),
    list(Gemeenten = "GM2", k_1ePlaatsing_1 = NA),   # CBS stuurt null
    list(Gemeenten = "GM3", k_1ePlaatsing_1 = 0),
    list(Gemeenten = "GM4", k_1ePlaatsing_1 = -3)))))
  expect_equal(iv3_gemeenten_met_data("T", 2025, "rekening"), 2L)
})

test_that("zonder lijst van de nachtelijke run: de reservelijst", {
  withr::local_options(cultuur.voorberekend_url = NA)
  expect_equal(iv3_keuzes(), IV3_KEUZES)
  expect_equal(iv3_tabel(2024), "45067NED")
  expect_error(iv3_tabel(1999), "Geen Iv3-tabel")
})

test_that("met lijst van de nachtelijke run: die lijst en zijn tabellen", {
  map <- withr::local_tempdir()
  withr::local_options(cultuur.voorberekend_url =
                         paste0("file:///", normalizePath(map, winslash = "/")))
  rm(list = ls(voorberekend_geheugen), envir = voorberekend_geheugen)
  withr::defer(rm(list = ls(voorberekend_geheugen), envir = voorberekend_geheugen))
  saveRDS(data.frame(keuze = "2025_rekening", label = "Jaarrekening 2025",
                     jaar = 2025L, soort = "rekening", tabel = "45071NED"),
          file.path(map, "iv3_keuzes.rds"))
  expect_equal(iv3_keuzes(), c("Jaarrekening 2025" = "2025_rekening"))
  expect_equal(iv3_tabel(2025), "45071NED")
})
