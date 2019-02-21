testthat::test_that(
  "Tests returns NULL from load_experiment", {
    sir1[, ] <- NULL
    testthat::expect_equal(rama:::get_parameters(sir1), NULL)
    testthat::expect_equal(rama:::get_variables(sir1), NULL)
    test <- rama:::make_df_dic(NULL)
    testthat::expect_equal(dim(test$out), c(0, 0))
    testthat::expect_equal(test$dic, NULL)
})
