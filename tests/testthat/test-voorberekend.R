opt <- list(context = TRUE, woordvormen = FALSE, dedup = TRUE)

test_that("zoek_sleutel hangt niet af van volgorde of getaltype", {
  a <- zoek_sleutel(c("b", "a"), c(2020, 2026), opt)
  expect_equal(a, zoek_sleutel(c("a", "b"), c(2020L, 2026L), opt))
  expect_false(a == zoek_sleutel(c("a", "b"), c(2021L, 2026L), opt))
  expect_false(a == zoek_sleutel(c("a", "b"), c(2020L, 2026L),
                                 modifyList(opt, list(dedup = FALSE))))
})

test_that("haal_resultaat gebruikt voorberekende data als die er is", {
  map <- tempfile()
  dir.create(map)
  nep <- list(totaal_docs = 42, termen = c("a", "b"))
  saveRDS(nep, file.path(map, sprintf("zoek_%s.rds",
                                      zoek_sleutel(c("a", "b"), c(2020, 2026), opt))))
  withr::local_options(cultuur.voorberekend_url =
                         paste0("file:///", normalizePath(map, winslash = "/")))
  voorberekend_cache$reset()

  res <- haal_resultaat(c("b", "a"), c(2020, 2026), opt)
  expect_equal(res$totaal_docs, 42)
  expect_equal(res$bron, "voorberekend")
  expect_null(lees_voorberekend("bestaat_niet.rds"))
})

test_that("zonder URL wordt er niets voorberekends gelezen", {
  withr::local_options(cultuur.voorberekend_url = NA)
  expect_null(lees_voorberekend("zoek_x.rds"))
})
