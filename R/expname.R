# expname ----------------------------------------------------------------------
#' Get the name of an experiment
#'
#' Retrieves the name of the experiment that an \code{experiment} object is
#' linked to.
#'
#' @param x An object of class \code{experiment}.
#'
#' @return The name of the experiment that the inputed \code{experiment} object
#' is linked to.
#'
#' @examples
#' exp1 <- load_experiment("sir", system.file("examples", "sir.gaml", package = "rama"))
#' expname(exp1)
#'
#' @export
expname <- function(x) UseMethod("expname")

#' @rdname expname
#' @export
expname.default <- function(x) "Unknown class"

#' @rdname expname
#' @export
expname.experiment <- function(x) attributes(x)$experiment
