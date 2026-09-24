test_that("cultuurcontext geldt alleen voor termen die zelf niet over cultuur gaan", {
  opt <- list(context = TRUE, woordvormen = FALSE, dedup = TRUE)
  q <- term_query("talentontwikkeling", opt)
  expect_named(q, "intervals")
  expect_equal(q$intervals$text$all_of$max_gaps, CONTEXT_AFSTAND)
  expect_named(term_query("amateurkunst", opt), "multi_match")
  expect_named(term_query("talentontwikkeling", list(context = FALSE)), "multi_match")
})

test_that("woordvormen alleen voor enkele woorden, met escaping", {
  opt <- list(context = FALSE, woordvormen = TRUE)
  expect_equal(term_query("amateurkunst", opt)$query_string$query, "amateurkunst*")
  expect_equal(term_query("sint-jan", opt)$query_string$query, "sint\\-jan*")
  expect_named(term_query("kunst en cultuur", opt), "multi_match")
  ctx <- term_query("talentontwikkeling", list(context = TRUE, woordvormen = TRUE))
  expect_named(ctx$intervals$text$all_of$intervals[[1]], "prefix")
})

test_that("tel gebruikt unieke bijlagen plus documenten zonder bestand", {
  b <- list(doc_count = 100, uniek = list(value = 70), zonder = list(doc_count = 5))
  expect_equal(tel(b, dedup = TRUE), 75)
  expect_equal(tel(b, dedup = FALSE), 100)
  expect_equal(tel(NULL, TRUE), 0)
})

test_that("zonder dedup geen extra sub-aggregaties", {
  expect_null(telling_aggs(FALSE))
  expect_null(treffer_filters("x", list(dedup = FALSE))$aggs)
  expect_named(treffer_filters("x", list(dedup = TRUE))$aggs, c("uniek", "zonder"))
})
