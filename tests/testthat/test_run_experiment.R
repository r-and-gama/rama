testthat::test_that(
  "Tests returns correct output from run_experiment", {

    df <-   data.frame(S0 = 900,
                       I0 = 100,
                       R0 = 0,
                       beta = 1.5,
                       gamma = .15,
                       S = 1,
                       I = 2,
                       R = 5,
                       nbiter = 10,
                       seed = "123456789")

    # Check error
    testthat::expect_error(run_experiment(df),
                           "The argument \"exp\" is not an object of class \"experiment\".")

    # Check realexp
    exp0 <- as_experiment(df, parameters = c("S0", "I0", "R0", "beta", "gamma"),
                          obsrates  = c("S", "I", "R"),
                          tmax = "nbiter", seed = "seed", experiment = "sir",
                          model =
                            system.file("models", "sir.gaml", package = "rama"))
    otp <- run_experiment(exp0)
    testthat::expect_equal(otp$r_I, 2)

  })
