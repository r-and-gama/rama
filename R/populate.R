# sir1 <- load_experiment("sir", system.file("examples", "sir.gaml", package = "rama"), "sir")


populate <- function(xprmt, ...) {
  args <- as.list(match.call(expand.dots = FALSE))
  values <- args$`...`
  to_expand <- as.data.frame(xprmt)
  list(values, to_expand)
}

a <- populate(sir1, p_S0 = 1:3, p_I0 = 5:6)
values <- a[[1]]
to_expand <- a[[2]]

the_names <- names(to_expand)


slots <- names(values)
