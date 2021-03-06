library(dplyr)

test_that("Tests tidyverse", {
	df <- data.frame("S0" = rep(999, 5), "I0" = rep(1, 5), "R0" = rep(0, 5),"beta" = rep(1.5, 5), "gamma" = runif (5, 0, 1), "S" = rep(1, 5), "I" = rep(1, 5), "R" = c(1: 5), "a" = rep(1000, 5), "b" = rep(1, 5)) 
	exp <- as_experiment(df, parameters = c("S0", "I0", "R0", "beta", "gamma"),obsrates = c("S", "I", "R"), tmax = "a", seed = "b", experiment = "sir",  model = system.file("models", "sir.gaml", package = "rama"))
	testthat::expect_s3_class(arrange(exp), "experiment")
	testthat::expect_s3_class(distinct(exp), "experiment")
	testthat::expect_s3_class(filter(exp), "experiment")
	testthat::expect_s3_class(groups(exp), "experiment")
	testthat::expect_s3_class(mutate(exp), "experiment")
	testthat::expect_s3_class(select(exp), "experiment")
	testthat::expect_s3_class(slice(exp), "experiment")
	testthat::expect_s3_class(summarise(exp), "experiment")
	testthat::expect_s3_class(transmute(exp), "experiment")
	testthat::expect_s3_class(ungroup(exp), "experiment")
})
