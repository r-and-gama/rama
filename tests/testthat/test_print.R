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

  # Check realexp
  exp0 <- as_experiment(df, parameters = c("S0", "I0", "R0", "beta", "gamma"),
                        obsrates  = c("S", "I", "R"),
                        tmax = "nbiter", seed = "seed", experiment = "sir",
                        model =
                          system.file("models", "sir.gaml", package = "rama"))

  exp1 <- exp0
  exp1[,] <- NULL
  testthat::expect_equal(capture.output(exp1)[1],
                         "Experiment without any simulation, tunable parameter or observed variable")

  exp <- load_experiment("prey_predator", system.file("models",
                                                      "NicolasPred.gaml",
                                                      package = "rama"))
  testthat::expect_length(capture.output(repl(exp, 100)), 39)

  exp3 <- as_experiment(df, parameters = c("S0", "I0"),
                        obsrates  = c("S", "I"),
                        tmax = "nbiter", seed = "seed", experiment = "sir",
                        model =
                          system.file("models", "sir.gaml", package = "rama"))

  testthat::expect_length(capture.output(exp3), 9)
  testthat::expect_length(capture.output(exp0), 9)

})
