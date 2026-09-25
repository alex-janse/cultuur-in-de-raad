pg_voorbeeld <- function() {
  tibble(key = c("a", "b", "c", "d", "e"), gemeente = c("A", "B", "C", "D", "E"),
         gemeentecode = sprintf("GM%04d", 1:5),
         inwoners = c(15000, 30000, 35000, 45000, 150000),
         totaal = c(40, 50, 60, 5, 80), per_1000 = c(4, 3, 2, 9, 1),
         per_1000_laag = 0, per_1000_hoog = 10,
         weinig_treffers = c(FALSE, FALSE, FALSE, TRUE, FALSE))
}

test_that("grootteklasse en rang binnen de klasse", {
  expect_equal(grootteklasse(c(15000, 20000, 99999, 150000, NA)),
               c("tot 20.000 inwoners", "20.000–50.000 inwoners",
                 "50.000–100.000 inwoners", "100.000+ inwoners", NA))
  r <- rangschik(pg_voorbeeld(), "relatief", "20.000–50.000 inwoners")
  expect_equal(r$key, c("b", "c", "d"))       # d: weinig treffers, onderaan
  expect_equal(r$rang, c(1L, 2L, NA))
  expect_equal(nrow(rangschik(pg_voorbeeld(), "relatief", "")), 5)
})

test_that("percentiel telt alleen gemeenten met een rang", {
  r <- rangschik(pg_voorbeeld(), "relatief")
  expect_equal(percentiel(r, "a"), 100)       # hoogste van de 4 met rang
  expect_equal(percentiel(r, "e"), 0)
  expect_true(is.na(percentiel(r, "d")))      # geen rang
})

test_that("trend_richting: alleen bij genoeg jaren en treffers", {
  trend <- tibble(jaar = 2020:2025, term = "__alle__",
                  n = c(10, 10, 12, 15, 20, 20), archief = 1000)
  r <- trend_richting(trend, jaar_nu = 2026)
  expect_equal(r$tekst, "toegenomen")
  expect_equal(r$van, "2020–2021")
  expect_equal(r$tot, "2024–2025")
  expect_null(trend_richting(trend |> mutate(n = 1), jaar_nu = 2026))  # te weinig
  expect_null(trend_richting(trend[1:3, ], jaar_nu = 2026))
})

test_that("verhaal_gemeente: vergelijking, klasse, budget en voorzichtigheid", {
  pg <- pg_voorbeeld()
  trend <- tibble(jaar = 2020:2025, term = "__alle__",
                  n = c(20, 20, 18, 12, 10, 10), archief = 1000)
  budget <- tibble(gemeentecode = pg$gemeentecode,
                   cultuur_per_inw = c(50, 60, 70, 80, 90))
  z <- verhaal_gemeente(pg[1, ], pg, trend, budget, "Jaarrekening 2024")
  expect_match(z[1], "A bespreekt deze thema's in verhouding vaker dan 100%")
  expect_match(z[2], "afgenomen")
  expect_match(z[3], "^Volgens de jaarrekening 2024 gaf de gemeente €50 per inwoner aan cultuur uit; de mediaan van alle gemeenten is €70")

  # weinig treffers: geen vergelijking
  z_d <- verhaal_gemeente(pg[4, ], pg, trend[0, ], NULL)
  expect_match(z_d[1], "te weinig")
  expect_length(z_d, 1)
})

test_that("het tabblad Uitleg bouwt zonder fouten", {
  expect_s3_class(uitleg_ui(), "shiny.tag")
})
