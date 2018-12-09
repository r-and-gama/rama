# Defines the GAMA repository --------------------------------------------------

gama_repo <- function(repo = NULL) {
  if (! is.null(repo)) options(rama.repo = repo)
}
