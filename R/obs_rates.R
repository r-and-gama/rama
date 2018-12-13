# obs_rates ------------------------------------------------------------------
#' Extract monitoring rates
#'
#' Subsets the columns of an \code{experiment} object that correspond to the
#' obs_rates rates of the monitored variables.
#'
#' @param x An object of class \code{experiment}.
#'
#' @return A data frame that is a subset of the inputed \code{experiment} object.
#'
#' @examples
#' exp1 <- load_experiment("sir", system.file("examples", "sir.gaml",
#'                                             package = "rama"), "sir")
#' exp2 <- repl(exp1, 10)
#' obs_rates(exp2)
#'
#' @export
obs_rates <- function(x) UseMethod("obs_rates")

#' @rdname obs_rates
#' @export
obs_rates.default <- function(x) "Unknown class"

#' @rdname obs_rates
#' @export
obs_rates.experiment <- function(x) {
  as.data.frame(x[, grep("^r_", names(x), value = TRUE), drop = FALSE])
}
