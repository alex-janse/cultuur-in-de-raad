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
