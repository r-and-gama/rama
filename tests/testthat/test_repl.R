testthat::test_that("Tests returns error/warning from repl", {
  test <- "test"
  testthat::expect_equal(repl(test), "Unknown class")
  testthat::expect_equal(repl(sir1), sir1)
  sir1$seed <- c(1:2)
  testthat::expect_warning(repl(sir1, times = c("1" = 3, "4" = 5)),
                           'Row "4" requested is out of bound. Rows of NAs are returned')
})
