trend_df <- function() {
  expand.grid(gebied = c("Nederland", "Tilburg"), jaar = 2020:2022,
              term = c("__alle__", "amateurkunst", "cultuurbeoefening"),
              stringsAsFactors = FALSE) |>
    as_tibble() |>
    mutate(n = seq_along(jaar), archief = 1000)
}

test_that("plot_trend maakt een paneel per term, plus het totaal", {
  p <- plot_trend(trend_df(), c("amateurkunst", "cultuurbeoefening"), c(2020, 2022))
  b <- ggplot_build(p)
  expect_equal(length(unique(b$layout$layout$PANEL)), 3)
  expect_equal(levels(p$data$term)[1], ALLE_TERMEN_LABEL)
})

test_that("plot_trend met alleen_totaal werkt ook bij één term", {
  df <- trend_df() |> filter(term != "cultuurbeoefening")
  p <- plot_trend(df, "amateurkunst", c(2020, 2022), alleen_totaal = TRUE)
  expect_equal(unique(as.character(p$data$term)), ALLE_TERMEN_LABEL)
  expect_gt(nrow(p$data), 0)
  p1 <- plot_trend(df, "amateurkunst", c(2020, 2022))
  expect_equal(unique(as.character(p1$data$term)), "amateurkunst")
})

test_that("relatief deelt door het archief en negeert heel kleine jaren", {
  df <- tibble(gebied = "X", jaar = 2020:2021, term = "a", n = c(5, 5),
               archief = c(1000, 10))
  p <- plot_trend(df, "a", c(2020, 2021))
  expect_equal(p$data$waarde, c(5, NA))
})

test_that("maak_budget_df koppelt en deelt gemeenten in profielen in", {
  pg <- tibble(gemeentecode = sprintf("GM%04d", 1:6), gemeente = LETTERS[1:6],
               inwoners = c(5e4, 5e4, 5e4, 5e4, 5e4, 1e3),
               per_1000 = c(1, 2, 3, 4, 5, 6), per_100k = 1)
  lasten <- tibble(gemeentecode = sprintf("GM%04d", 1:6),
                   cultuur_per_inw = c(100, 80, 60, 40, 20, 10))
  df <- maak_budget_df(pg, lasten, "per_1000")
  expect_equal(nrow(df), 5)  # gemeente F is te klein
  expect_equal(df$profiel[df$gemeente == "E"], "Veel aandacht, weinig budget")
  expect_equal(df$profiel[df$gemeente == "A"], "Veel budget, weinig aandacht")
  expect_s3_class(plot_budget(df, c(2020, 2026), "y", "Jaarrekening 2024"), "ggplot")
})

test_that("plot_budget_trend toont gemeente naast de mediaan", {
  lasten <- tibble(gemeentecode = rep(c("GM0001", "GM0002", "GM0003"), 2),
                   keuze = rep(c("2023_rekening", "2024_rekening"), each = 3),
                   jaar = rep(2023:2024, each = 3),
                   cultuur_per_inw = c(50, 100, 150, 60, 110, 160))
  p <- plot_budget_trend(lasten, "GM0001", "Aa")
  expect_setequal(unique(p$data$reeks), c("Aa", "Mediaan alle gemeenten"))
  expect_equal(p$data$euro[p$data$reeks == "Mediaan alle gemeenten"], c(100, 110))
  expect_no_error(ggplot_build(p))
})

test_that("het lopende jaar krijgt een open punt en een toelichting", {
  df <- tibble(gebied = "Nederland", jaar = (huidig_jaar() - 2):huidig_jaar(),
               term = "a", n = c(5, 6, 2), archief = 1000)
  p <- plot_trend(df, "a", c(huidig_jaar() - 2, huidig_jaar()))
  expect_equal(p$data$lopend, c(FALSE, FALSE, TRUE))
  expect_match(p$labels$caption, "nog niet compleet")
  expect_no_error(ggplot_build(p))
  vroeger <- plot_trend(df |> mutate(jaar = jaar - 5), "a", c(2015, 2020))
  expect_null(vroeger$labels$caption)
})
