# Periodeblokken: opgeteld uit de cijfers per jaar van een resultaat over alle jaren

volledig <- function() {
  trends <- tidyr::expand_grid(key = c("a", "b"), jaar = 2016:2025,
                               term = c("__alle__", "x", "y")) |>
    mutate(n = ifelse(term == "__alle__", 10, 6),
           archief = 1000)
  # b heeft pas archief vanaf 2020
  trends$archief[trends$key == "b" & trends$jaar < 2020] <- 0
  trends$n[trends$key == "b" & trends$jaar < 2020] <- 0
  list(
    termen = c("x", "y"), jaren = c(2016L, 2025L), opties = STANDAARD_OPTIES,
    trends = trends,
    jaren_nl = trends |> group_by(jaar, term) |>
      summarise(n = sum(n), archief = sum(archief), .groups = "drop"),
    per_gemeente = tibble(key = c("a", "b"), gemeente = c("A", "B"),
                          gemeentecode = c("GM1", "GM2"), inwoners = c(1e5, 5e4)),
    docs_jaren = tibble(key = c("a", "b", "a"), titel = c("t1", "t2", "t3"),
                        datum = c("2017-03-01", "2021-05-01", "2024-01-01"),
                        link = "https://x.nl"),
    berekend_op = Sys.time(), schema = SCHEMA_VERSIE
  )
}

test_that("blok_jaren en blok_indeling", {
  expect_equal(blok_jaren("2018-2021"), c(2018L, 2021L))
  expect_equal(blok_jaren("2026-nu"), c(2026L, huidig_jaar()))
  expect_equal(blok_indeling("2021-2024"), "Landelijke cultuurperiodes")
  expect_equal(blok_indeling(STANDAARD_BLOK), "Raadsperiodes")
  expect_true(all(vapply(unlist(PERIODES), \(b) length(blok_jaren(b)) == 2, logical(1))))
})

test_that("blok_resultaat telt alleen de jaren van het blok", {
  r <- blok_resultaat(volledig(), c(2018L, 2021L))
  a <- r$per_gemeente[r$per_gemeente$key == "a", ]
  b <- r$per_gemeente[r$per_gemeente$key == "b", ]
  expect_equal(a$totaal, 40)
  expect_equal(a$n_x, 24)
  expect_equal(a$archief, 4000)
  expect_equal(a$jaren_dekking, 4)
  # b heeft in dit blok alleen 2020 en 2021
  expect_equal(b$jaren_dekking, 2)
  expect_equal(b$per_1000, 10)
  expect_equal(r$jaren, c(2018L, 2021L))
  expect_equal(r$periode_volledig, c(2016L, 2025L))
  expect_equal(r$totaal_docs, 60)
  # Trends blijven over alle jaren, voor de trendgrafiek
  expect_equal(range(r$trends$jaar), c(2016L, 2025L))
  # Documenten uit het blok, met gemeentenaam
  expect_equal(r$docs$titel, "t2")
  expect_equal(r$docs$gemeente, "B")
})

test_that("een gemeente zonder archief in het blok valt weg", {
  r <- blok_resultaat(volledig(), c(2016L, 2017L))
  expect_equal(r$per_gemeente$key, "a")
})

test_that("haal_voorbeelden leest fragmenten per gemeente en documenten per jaar", {
  hit <- \(index, datum, titel) list(`_index` = index, `_source` = list(
    name = titel, last_discussed_at = datum, original_url = "https://x.nl"),
    highlight = list(text = list("een ⟦term⟧ hier")))
  json <- list(
    `_shards` = list(failed = 0, total = 1),
    aggregations = list(
      gemeenten = list(sum_other_doc_count = 0, buckets = list(
        list(key = "ori_utrecht_20240101", nieuwste = list(hits = list(hits = list(
          hit("ori_utrecht_20240101", "2025-02-01", "Nieuw"),
          hit("ori_utrecht_20240101", "2024-02-01", "Ouder"))))),
        list(key = "ori_brielle_20200101", nieuwste = list(hits = list(hits = list(
          hit("ori_brielle_20200101", "2021-02-01", "Bezwaar van bewoner"))))))),
      jaren = list(buckets = list(
        list(key_as_string = "2025", nieuwste = list(hits = list(hits = list(
          hit("ori_utrecht_20240101", "2025-02-01", "Nieuw")))))))
    )
  )
  httr2::local_mocked_responses(function(req) httr2::response_json(body = json))
  v <- haal_voorbeelden("term", c(2016L, 2025L), per_gemeente = 1)
  expect_equal(v$fragmenten$key, c("utrecht", "voorne_aan_zee"))
  expect_equal(v$fragmenten$titel[1], "Nieuw")     # alleen de nieuwste
  expect_true(v$fragmenten$privacy[2])
  expect_equal(nrow(v$docs), 1)
})

test_that("de trendgrafiek krijgt vlakken en lijnen bij een markering", {
  df <- volledig()$jaren_nl |> mutate(gebied = "Nederland")
  m <- list(blok = c(2018L, 2021L), corona = CORONA, lijnen = VERKIEZINGSJAREN,
            lijn_uitleg = "gemeenteraadsverkiezingen")
  p <- plot_trend(df, c("x", "y"), c(2016L, 2025L), markering = m)
  lagen <- vapply(p$layers, \(l) class(l$geom)[1], "")
  expect_equal(sum(lagen == "GeomRect"), 2)
  expect_true("GeomVline" %in% lagen)
  expect_match(p$labels$caption, "gemeenteraadsverkiezingen")
  # Zonder markering geen extra lagen
  kaal <- plot_trend(df, c("x", "y"), c(2016L, 2025L))
  expect_false(any(vapply(kaal$layers, \(l) class(l$geom)[1], "") == "GeomRect"))
})

test_that("periode_delen volgt de raadsperiodes", {
  expect_equal(periode_delen(c(2010L, 2026L), 8),
               list(c(2010L, 2017L), c(2018L, 2025L), c(2026L, 2026L)))
  expect_equal(periode_delen(c(2010L, 2013L), 8), list(c(2010L, 2013L)))
})

test_that("voeg_delen_samen geeft hetzelfde als één resultaat over alle jaren", {
  heel <- volledig()
  deel <- function(van, tot) {
    d <- heel
    d$trends <- heel$trends |> filter(jaar >= van, jaar <= tot)
    d$per_gemeente <- heel$per_gemeente |> mutate(ruw = as.list(paste0(key, van)))
    d$docs <- tibble(key = "a", titel = paste("doc", van), datum = paste0(tot, "-06-01"),
                     link = "", gemeente = "A")
    d$inwoners_ok <- TRUE
    d$docs_jaren <- NULL   # komt pas later uit haal_voorbeelden
    d
  }
  samen <- voeg_delen_samen(list(deel(2016L, 2019L), deel(2020L, 2025L)), c(2016L, 2025L))
  a <- samen$per_gemeente[samen$per_gemeente$key == "a", ]
  expect_equal(a$totaal, 100)              # 10 jaar x 10
  expect_equal(a$jaren_dekking, 10)
  expect_equal(samen$jaren, c(2016L, 2025L))
  expect_null(samen$periode_volledig)
  expect_equal(range(samen$trends$jaar), c(2016L, 2025L))
  # De indexnamen van alle delen blijven bewaard (voor stadsdelen en fusies)
  expect_setequal(a$ruw[[1]], c("a2016", "a2020"))
  expect_equal(samen$docs$titel[1], "doc 2020")
  # Een blok uit het samengevoegde resultaat klopt ook
  expect_equal(blok_resultaat(samen, c(2018L, 2021L))$per_gemeente$totaal[1], 40)
})
