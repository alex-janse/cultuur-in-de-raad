test_that("maak_kaart_df: echte nul, te weinig gegevens en geen archief verschillen", {
  vierkant <- function(x) sf::st_polygon(list(rbind(c(x, 0), c(x + 1, 0),
                                                    c(x + 1, 1), c(x, 1), c(x, 0))))
  grenzen <- sf::st_sf(gemeentecode = c("GM0001", "GM0002", "GM0003", "GM0004"),
                       grensnaam = c("Aa", "Bb", "Cc", "Dd"),
                       geometry = sf::st_sfc(vierkant(0), vierkant(2), vierkant(4),
                                             vierkant(6), crs = 4326))
  per_gemeente <- tibble(gemeentecode = c("GM0001", "GM0002", "GM0003"),
                         key = c("aa", "bb", "cc"), gemeente = c("Aa", "Bb", "Cc"),
                         totaal = c(10, 0, 3), per_1000 = c(2.5, 0, NA),
                         per_100k = c(40, 0, NA),
                         weinig_treffers = c(FALSE, TRUE, TRUE))
  df <- maak_kaart_df(grenzen, per_gemeente, "relatief")
  expect_equal(nrow(df), 4)
  expect_equal(df$waarde, c(2.5, 0, NA, NA))   # Bb: echte nul
  expect_match(df$label[2], "te weinig voor een rang")
  expect_match(df$label[3], "te weinig documenten")
  expect_match(df$label[4], "geen raadsarchief")
  expect_equal(maak_kaart_df(grenzen, per_gemeente, "inwoners")$waarde[1], 40)
  expect_s3_class(voeg_kaartlagen_toe(leaflet(), df, "relatief"), "leaflet")
})

test_that("labels in de kaart zijn HTML-veilig", {
  vierkant <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  grenzen <- sf::st_sf(gemeentecode = "GM0001", grensnaam = "<script>x</script>",
                       geometry = sf::st_sfc(vierkant, crs = 4326))
  df <- maak_kaart_df(grenzen, tibble(gemeentecode = character(), key = character(),
                                      gemeente = character(), totaal = numeric(),
                                      per_1000 = numeric(),
                                      weinig_treffers = logical()), "relatief")
  expect_false(grepl("<script>", df$label))
})
