test_that("Tests package examples", {
  skip_on_travis()
  testthat::test_examples("man/")
})
