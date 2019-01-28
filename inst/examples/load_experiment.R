# Listing the models available in the "examples" directory of the "rama" library:
grep(".gaml", dir(system.file("models", package = "rama")), value = TRUE)
# Loading experiment "sir" from the "sir.gaml" file:
exp1 <- load_experiment("sir",
                        system.file("models", "sir.gaml", package = "rama"))
# Checking the class:
class(exp1)
