test_that("is_cultuurterm herkent cultuurtermen alleen aan het begin van een woord", {
  expect_true(is_cultuurterm("amateurkunst"))        # in CULTUURWOORDEN
  expect_true(is_cultuurterm("cultuureducatie"))
  expect_true(is_cultuurterm("kunst en cultuur"))
  expect_true(is_cultuurterm("podiumkunsten"))
  expect_false(is_cultuurterm("bedrijfscultuur"))    # cultuur midden in het woord
  expect_false(is_cultuurterm("kunstgras"))          # uitgezonderd
  expect_false(is_cultuurterm("kunstwerken"))        # bruggen en viaducten
  expect_false(is_cultuurterm("talentontwikkeling"))
  expect_false(is_cultuurterm("bibliotheek"))
})

test_that("dekking: jaren met archief, het lopende jaar naar rato", {
  trends <- tibble(
    key = c(rep("a", 3), rep("b", 3)),
    jaar = rep(2024:2026, 2), term = "__alle__", n = 1,
    archief = c(1000, 1000, 1000, 10, 1000, 1000))  # b: 2024 nog nauwelijks archief
  d <- dekking_per_gemeente(trends, vandaag = as.Date("2026-07-02"))
  fractie <- as.numeric(format(as.Date("2026-07-02"), "%j")) / 365
  expect_equal(d$eerste_jaar, c(2024, 2025))
  expect_equal(d$jaren_dekking, c(2 + fractie, 1 + fractie))
})

test_that("poisson_interval: breed bij kleine aantallen, 0 heeft ondergrens 0", {
  klein <- poisson_interval(4, 1000, 1000)
  groot <- poisson_interval(400, 100000, 1000)
  expect_lt(klein$laag, 4)
  expect_gt(klein$hoog, 4)
  expect_lt(groot$hoog - groot$laag, klein$hoog - klein$laag)
  expect_equal(poisson_interval(0, 1000, 1000)$laag, 0)
  # exact interval voor n = 4: 1,09 - 10,24
  expect_equal(round(klein$laag, 2), 1.09)
  expect_equal(round(klein$hoog, 2), 10.24)
})

test_that("bereken_maatstaven deelt door de jaren met dekking", {
  pg <- tibble(key = c("a", "b"), archief = c(5000, 5000), totaal = c(50, 5),
               inwoners = c(1e5, 1e5), jaren_dekking = c(5, 2.5))
  m <- bereken_maatstaven(pg)
  expect_equal(m$per_1000, c(10, 1))
  expect_equal(m$per_100k, c(10, 2))       # 50 / 5 jaar, 5 / 2,5 jaar
  expect_equal(m$weinig_treffers, c(FALSE, TRUE))
  expect_true(all(m$per_1000_laag < m$per_1000 & m$per_1000_hoog > m$per_1000))

  # te weinig archief voor het aantal gedekte jaren -> geen relatieve waarde
  klein <- bereken_maatstaven(tibble(key = "c", archief = 300, totaal = 5,
                                     inwoners = 1e5, jaren_dekking = 2))
  expect_true(is.na(klein$per_1000))
  # geen enkel gedekt jaar -> geen waarde
  leeg <- bereken_maatstaven(tibble(key = "d", archief = 9000, totaal = 5,
                                    inwoners = 1e5, jaren_dekking = NA_real_))
  expect_true(is.na(leeg$per_1000) && is.na(leeg$per_100k))
})

test_that("rangschik: echte nullen doen mee, weinig treffers krijgen geen rang", {
  pg <- tibble(key = c("a", "b", "c", "d"), gemeente = c("A", "B", "C", "D"),
               totaal = c(50, 5, 0, 30), per_1000 = c(2, 9, 0, 3),
               per_1000_laag = 1, per_1000_hoog = 4, inwoners = 30000,
               weinig_treffers = c(FALSE, TRUE, TRUE, FALSE))
  r <- rangschik(pg, "relatief")
  expect_equal(r$key, c("d", "a", "b", "c"))   # B (9) is hoog maar toevallig
  expect_equal(r$rang, c(1L, 2L, NA, NA))
  expect_equal(rang_tekst(r, "a"), "rang 2 van 2")
  expect_match(rang_tekst(r, "b"), "minder dan 10")
  expect_match(rang_tekst(r, "zz"), "niet gerangschikt")
})

test_that("periode_jaren telt het lopende jaar naar rato", {
  vandaag <- as.Date("2026-07-02")
  fractie <- as.numeric(format(vandaag, "%j")) / 365
  expect_equal(periode_jaren(c(2020, 2026), vandaag), 6 + fractie)
  expect_equal(periode_jaren(c(2020, 2024), vandaag), 5)
})
