gaml_file <- system.file("models", "sir.gaml", package = "rama")
exp1 <- load_experiment("sir", gaml_file, "sir")
save_to_gama(exp1)
