testthat::test_that(
  "Tests returns NULL from load_experiment", {
    sir1[, ] <- NULL
    testthat::expect_equal(rama:::get_parameters(sir1), NULL)
    testthat::expect_equal(rama:::get_variables(sir1), NULL)
})
