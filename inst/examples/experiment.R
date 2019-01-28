## experiment ##
exp0 <- experiment(
  data.frame(S0 = 999, I0 = 1, R0 = 0, beta = 1.5, gamma = .15),
  data.frame(S = 1, I = 1, R = 1),
  1000, 1, "sir", system.file("models", "sir.gaml", package = "rama"))
print(exp0)

## is.experiment ##
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
                       system.file("models", "sir.gaml", package = "rama"))
is.experiment(exp)



## `$<-` ##
# Here is an experiment with 1 simulation:
sir1 <- load_experiment("sir",
                        system.file("models", "sir.gaml", package = "rama"),
                        "sir")
sir1

# Let's replace the value of the "p_S0" column by a vector of 3 values:
sir2 <- sir1
sir2$p_S0 <- 1:3
# We can check that it automatically expands the number of simulations:
sir2

# If, on the contrary, we now replace the values of "p_S0" of "sir2" by a
# single value:
sir3 <- sir2
sir3$p_S0 <- 2
# We can check that it automatically reduces the number of simulations (if
# the replacement leads to an experiment with exactly identical simulations):
sir3
# If you wish to delete one column:
sir3$r_R <- NULL
sir3

# You can do the operation successively for 2 columns:
sir4 <- sir1
sir4
sir4$p_S0 <- c(1, 10, 100)
sir4
sir4$p_beta <- c(1.3, 1.7)
sir4
# which is equivalent to
fullfact(sir1, p_S0 = c(1, 10, 100), p_beta = c(1.3, 1.7))


