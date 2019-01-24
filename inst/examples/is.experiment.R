# to test if an object is a class `experiment`
df <- data.frame("S0" = rep(999, 5), "I0" = rep(1, 5), "R0" = rep(0, 5),
                "beta" = rep(1.5, 5), "gamma" = runif (5, 0, 1),
                "S" = rep(1, 5), "I" = rep(1, 5), "R" = rep(1, 5),
                "a" = rep(1000, 5), "b" = rep(1, 5))
exp <- as_experiment(df, parameters = c("S0", "I0", "R0", "beta", "gamma"),
                     obsrates = c("S", "I", "R"),
                     tmax = "a",
                     seed = "b",
                     experiment = "sir",
                     model =
                       system.file("examples", "sir.gaml", package = "rama"))
is.experiment(exp)
