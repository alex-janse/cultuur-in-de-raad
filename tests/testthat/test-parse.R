termen <- c("amateurkunst", "cultuurbeoefening")

test_that("parse_jaren maakt een lange tabel met ontbrekende buckets als 0", {
  buckets <- jsonlite::read_json(test_path("fixtures", "jaren.json"))
  df <- parse_jaren(buckets, termen)
  expect_equal(nrow(df), 6)
  expect_equal(df$n[df$jaar == 2024 & df$term == "__alle__"], 12)
  expect_equal(df$n[df$jaar == 2025 & df$term == "cultuurbeoefening"], 0)
  expect_equal(unique(df$archief[df$jaar == 2025]), 2000)
})

test_that("parse_jaren kan met een lege respons omgaan", {
  expect_equal(nrow(parse_jaren(list(), termen)), 0)
})

test_that("zoekvraag bevat per term een filter en de periode", {
  f <- treffer_filters(termen)$filters$filters
  expect_named(f, c("__alle__", termen))
  q <- periode_query(c(2020, 2021))
  expect_equal(q$bool$filter[[1]]$range$last_discussed_at$lt, "2022-01-01")
})
