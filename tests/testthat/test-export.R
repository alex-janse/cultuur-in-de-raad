test_that("schrijf_csv maakt een Excel-vriendelijke CSV", {
  f <- tempfile(fileext = ".csv")
  schrijf_csv(tibble(gemeente = "Súdwest-Fryslân", waarde = 1.5), f)
  bytes <- readBin(f, "raw", 3)
  expect_equal(bytes, as.raw(c(0xef, 0xbb, 0xbf)))
  regels <- readLines(f, encoding = "UTF-8", warn = FALSE)
  expect_match(regels[2], "Súdwest-Fryslân\";1,5", fixed = TRUE)
})

test_that("veilige_link laat alleen http(s) door", {
  expect_equal(veilige_link(c("https://a.nl/x", "javascript:alert(1)", NA)),
               c("https://a.nl/x", "", ""))
})
