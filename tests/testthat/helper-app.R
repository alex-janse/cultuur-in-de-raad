# Laadt global.R en R/*.R zoals Shiny dat doet, zonder de app te starten.
shiny::loadSupport(testthat::test_path("..", ".."), renv = globalenv())
