exp1 <- load_experiment("sir", system.file("examples", "sir.gaml",
                        package = "rama"))
model(exp1)
model(exp1) <-  system.file("examples", "CopyOfsir.gaml",
                        package = "rama")
model(exp1)
