# parameters -------------------------------------------------------------------
#' Extract parameters values
#'
#' Subsets the columns of an \code{experiment} object that correspond to the
#' values of the parameters.
#'
#' @param x An object of class \code{experiment}.
#'
#' @return A data frame that is a subset of the inputed \code{experiment} object.
#'
#' @examples
#' exp1 <- load_experiment("sir", system.file("examples", "sir.gaml",
#'                                           package = "rama"), "sir")
#' exp2 <- repl(exp1, 10)
#' parameters(exp2)
#'
#' @export
parameters <- function(x) UseMethod("parameters")

#' @rdname parameters
#' @export
parameters.default <- function(x) "Unknown class"

#' @rdname parameters
#' @export
parameters.experiment <- function(x) {
  as.data.frame(x[, grep("^p_", names(x), value = TRUE), drop = FALSE])
}
