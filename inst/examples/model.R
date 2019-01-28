exp1 <- load_experiment("sir", system.file("models", "sir.gaml",
                        package = "rama"))
model(exp1)
model(exp1) <-  system.file("models", "CopyOfsir.gaml",
                        package = "rama")
model(exp1)
