test_that("maak_kaart_df koppelt op gemeentecode en kiest de juiste maatstaf", {
  vierkant <- function(x) sf::st_polygon(list(rbind(c(x, 0), c(x + 1, 0),
                                                    c(x + 1, 1), c(x, 1), c(x, 0))))
  grenzen <- sf::st_sf(gemeentecode = c("GM0001", "GM0002", "GM0003"),
                       grensnaam = c("Aa", "Bb", "Cc"),
                       geometry = sf::st_sfc(vierkant(0), vierkant(2), vierkant(4),
                                             crs = 4326))
  per_gemeente <- tibble(gemeentecode = c("GM0001", "GM0002"), key = c("aa", "bb"),
                         gemeente = c("Aa", "Bb"), totaal = c(10, 0),
                         per_1000 = c(2.5, NA), per_100k = c(40, 1))
  df <- maak_kaart_df(grenzen, per_gemeente, "relatief")
  expect_equal(nrow(df), 3)
  expect_equal(df$waarde, c(2.5, NA, NA))   # Bb: 0 treffers, Cc: geen archief
  expect_match(df$label[3], "geen")
  expect_equal(maak_kaart_df(grenzen, per_gemeente, "inwoners")$waarde[1], 40)
  expect_s3_class(voeg_kaartlagen_toe(leaflet(), df, "relatief"), "leaflet")
})
