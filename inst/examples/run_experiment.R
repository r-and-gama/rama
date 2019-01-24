#load experiment
gaml_file <- system.file("models", "sir.gaml", package = "rama")
exp1 <- load_experiment("sir", gaml_file, "sir")
# run experiment
out <- run_experiment(exp1)
