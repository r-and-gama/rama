#load experiment
gaml_file <- system.file("models", "sir.gaml", package = "rama")
exp1 <- load_experiment("sir", gaml_file, "sir")
# run experiment
out <- run_experiment(exp1)

# for a more complexe example
exp1 <- repl(exp1, 2)
exp1$r_R <- 2:3
output <- run_experiment(exp1)
str(output)
