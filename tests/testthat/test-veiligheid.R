test_that("markeer_html laat geen HTML uit de bron door, alleen <mark>", {
  x <- paste0("<script>alert(1)</script> de ", MARK_BEGIN, "amateurkunst", MARK_EIND,
              " in <b>Tilburg</b> & omstreken")
  h <- markeer_html(x)
  expect_false(grepl("<script>", h, fixed = TRUE))
  expect_false(grepl("<b>", h, fixed = TRUE))
  expect_match(h, "&lt;script&gt;", fixed = TRUE)
  expect_match(h, "<mark>amateurkunst</mark>", fixed = TRUE)
  expect_match(h, "&amp; omstreken", fixed = TRUE)
})

test_that("markeer_html verwijdert losse of geneste markeringen", {
  h <- markeer_html(paste0("a ", MARK_BEGIN, "b ", MARK_BEGIN, "c", MARK_EIND, " d", MARK_EIND))
  expect_equal(lengths(regmatches(h, gregexpr("<mark>", h))),
               lengths(regmatches(h, gregexpr("</mark>", h))))
  expect_false(grepl(MARK_BEGIN, h) || grepl(MARK_EIND, h))
})

test_that("CSV-export maakt formules onschadelijk", {
  expect_equal(excel_veilig(c("=HYPERLINK(\"x\")", "+31", "-2", "@SUM(A1)", "Tilburg", NA)),
               c("'=HYPERLINK(\"x\")", "'+31", "'-2", "'@SUM(A1)", "Tilburg", NA))
  f <- tempfile(fileext = ".csv")
  schrijf_csv(tibble(titel = "=1+1", aantal = -3), f)
  regel <- readLines(f, encoding = "UTF-8", warn = FALSE)[2]
  expect_match(regel, "\"'=1+1\";-3", fixed = TRUE)   # getal blijft een getal
})

test_that("privacyfilter herkent stukken met gegevens van burgers", {
  titels <- c("Bezwaarschrift tegen besluit", "Zienswijze bewoners Dorpsstraat",
              "Ingekomen brief over de muziekschool", "Insprekers commissie 12 mei",
              "Raadsvoorstel Cultuurnota 2025-2028", "Kadernota Cultuur")
  expect_equal(grepl(PRIVACY_TITELS, tolower(titels)),
               c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that("veilig_object accepteert gewone gegevens en weigert code", {
  expect_true(veilig_object(tibble(a = 1:3, b = letters[1:3], d = Sys.time())))
  expect_true(veilig_object(list(x = list(y = "z"), t = Sys.Date())))
  vierkant <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  expect_true(veilig_object(sf::st_sf(code = "GM0001",
                                      geometry = sf::st_sfc(vierkant, crs = 4326))))
  expect_false(veilig_object(list(a = 1, f = function() "boem")))
  expect_false(veilig_object(list(e = new.env())))
  expect_false(veilig_object(structure(1, attr = quote(system("x")))))
})

test_that("een voorberekend bestand met code wordt geweigerd", {
  map <- withr::local_tempdir()
  saveRDS(list(per_gemeente = function() "boem"), file.path(map, "zoek_x.rds"))
  withr::local_options(cultuur.voorberekend_url =
                         paste0("file:///", normalizePath(map, winslash = "/")))
  rm(list = ls(voorberekend_geheugen), envir = voorberekend_geheugen)
  expect_warning(x <- lees_voorberekend("zoek_x.rds"), "geweigerd")
  expect_null(x)
})
