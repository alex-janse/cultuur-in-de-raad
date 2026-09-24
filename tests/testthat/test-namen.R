test_that("naar_key normaliseert CBS- en indexnamen", {
  expect_equal(naar_key("Utrecht (gemeente)"), "utrecht")
  expect_equal(naar_key("Súdwest-Fryslân"), "sudwest_fryslan")
  expect_equal(naar_key("Midden-Delfland"), "midden_delfland")
  expect_equal(naar_key("Laren (NH.)"), "laren")
})

test_that("indexnamen worden gemeente-keys", {
  expect_equal(index_naar_ruw("ori_midden-delfland_20250525191105"), "midden-delfland")
  expect_equal(ruw_naar_key("midden-delfland"), "midden_delfland")
  expect_equal(ruw_naar_key("amsterdam_noord"), "amsterdam")
})

test_that("index_patroon voorkomt verwarring tussen gelijkende namen", {
  expect_equal(index_patroon("bergen"), "ori_bergen_2*")
  expect_equal(index_patroon(c("amsterdam", "amsterdam_oost")), "ori_amsterdam*")
})

test_that("fmt gebruikt Nederlandse notatie", {
  expect_equal(fmt(1234.5), "1.234,5")
  expect_equal(fmt(NA), "–")
})

test_that("archieven van opgeheven gemeenten tellen bij de opvolger", {
  expect_equal(ruw_naar_key("weesp"), "amsterdam")
  expect_equal(ruw_naar_key(c("cuijk", "grave", "utrecht")),
               c("land_van_cuijk", "land_van_cuijk", "utrecht"))
  expect_equal(ruw_naar_key("mill_en_st_hubert"), "land_van_cuijk")
})

test_that("schoon_termen haalt vreemde tekens en korte termen weg", {
  expect_equal(schoon_termen(c(" Cultuur*Coach! ", "x", "kunst  en cultuur", "cultuurcoach")),
               c("cultuurcoach", "kunst en cultuur"))
})
