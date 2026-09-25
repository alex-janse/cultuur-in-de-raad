# Posit Connect Cloud publiceert alleen wat in manifest.json staat. Vergeet je
# na een wijziging `rsconnect::writeManifest()` (zie CLAUDE.md), dan draait
# online een andere versie dan je denkt. Deze tests vangen dat af.

root <- test_path("..", "..")
manifest <- jsonlite::read_json(file.path(root, "manifest.json"))
app_bestanden <- c("app.R", file.path("R", list.files(file.path(root, "R"),
                                                      pattern = "[.]R$")))

test_that("manifest.json bevat alle app-bestanden met actuele inhoud", {
  expect_setequal(names(manifest$files), app_bestanden)
  for (f in app_bestanden) {
    expect_equal(manifest$files[[f]]$checksum,
                 unname(tools::md5sum(file.path(root, f))),
                 info = paste(f, "is gewijzigd: maak manifest.json opnieuw"))
  }
})

test_that("alle gebruikte pakketten staan in manifest.json", {
  code <- unlist(lapply(file.path(root, app_bestanden), readLines, warn = FALSE))
  code <- code[!grepl("^\\s*#", code)]
  via_library <- unlist(regmatches(
    code, gregexpr("(?<=library\\()[A-Za-z][A-Za-z0-9.]*", code, perl = TRUE)))
  via_dubbele_punt <- unlist(regmatches(
    code, gregexpr("[A-Za-z][A-Za-z0-9.]*(?=::)", code, perl = TRUE)))
  basis <- c("base", "utils", "tools", "stats", "grDevices", "graphics", "methods")
  gebruikt <- setdiff(unique(c(via_library, via_dubbele_punt)), basis)
  ontbreekt <- setdiff(gebruikt, names(manifest$packages))
  expect_equal(ontbreekt, character(), info = "maak manifest.json opnieuw")
})
