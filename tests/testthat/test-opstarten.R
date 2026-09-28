# Sneller openen: kaartcoördinaten afronden en bestanden vooraf ophalen

test_that("rond_grenzen_af rondt af en laat de vormen heel", {
  vierkant <- sf::st_polygon(list(rbind(c(5.123456789, 52.1), c(5.2, 52.1),
                                        c(5.2, 52.187654321), c(5.123456789, 52.1))))
  twee <- sf::st_multipolygon(list(list(rbind(c(4, 51), c(4.1, 51), c(4.1, 51.1), c(4, 51)))))
  g <- sf::st_sf(gemeentecode = c("GM1", "GM2"),
                 geometry = sf::st_sfc(vierkant, twee, crs = 4326))
  a <- rond_grenzen_af(g)
  expect_s3_class(a, "sf")
  expect_equal(sf::st_crs(a), sf::st_crs(g))
  expect_equal(as.character(sf::st_geometry_type(a)), c("POLYGON", "MULTIPOLYGON"))
  expect_equal(unname(sf::st_coordinates(a[1, ])[1, "X"]), 5.1235)
  expect_equal(unname(sf::st_coordinates(a[1, ])[3, "Y"]), 52.1877)
  expect_equal(nrow(sf::st_coordinates(a[2, ])), nrow(sf::st_coordinates(g[2, ])))
  expect_equal(a$gemeentecode, g$gemeentecode)
})

test_that("haal_voorberekend_vooraf vult het geheugen en slaat mislukte over", {
  withr::local_options(cultuur.voorberekend_url = "https://voorbeeld.nl/data")
  rm(list = ls(voorberekend_geheugen), envir = voorberekend_geheugen)
  withr::defer(rm(list = ls(voorberekend_geheugen), envir = voorberekend_geheugen))
  pad <- withr::local_tempfile(fileext = ".rds")
  saveRDS(data.frame(x = 1:3), pad)
  inhoud <- readBin(pad, "raw", file.size(pad))
  httr2::local_mocked_responses(function(req) {
    if (grepl("goed", req$url)) httr2::response(200, body = inhoud)
    else httr2::response(404)
  })
  n <- haal_voorberekend_vooraf(c("goed.rds", "weg.rds"))
  expect_equal(n, 1L)
  expect_equal(voorberekend_geheugen[["goed.rds"]]$data, data.frame(x = 1:3))
  expect_null(voorberekend_geheugen[["weg.rds"]])
  # Daarna leest lees_voorberekend het uit het geheugen, zonder verzoek
  httr2::local_mocked_responses(function(req) stop("geen verzoek verwacht"))
  expect_equal(lees_voorberekend("goed.rds"), data.frame(x = 1:3))
})

test_that("voorberekende_bestanden noemt alles wat de app nodig heeft", {
  b <- voorberekende_bestanden()
  expect_true(all(c("bronversies.rds", "gemeentegrenzen.rds") %in% b))
  expect_length(grep("^zoek_", b), length(voorberekende_sets()))
  expect_length(grep("^iv3_", b), length(IV3_KEUZES))
})
