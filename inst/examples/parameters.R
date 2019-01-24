exp1 <- load_experiment("sir",
                        system.file("examples", "sir.gaml", package = "rama"), "sir")
exp2 <- repl(exp1, 10)
parameters(exp2)
