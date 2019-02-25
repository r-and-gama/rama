exp1 <- load_experiment("sir",
                        system.file("models", "sir.gaml", package = "rama"))
exp2 <- repl(exp1, 10)
obs_rates(exp2)
