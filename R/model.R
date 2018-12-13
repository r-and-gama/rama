# model ------------------------------------------------------------------------
#' Get the model of an experiment
#'
#' These functions allow to get and set the path to the \code{.gaml} file that
#' contains the model of an \code{experiment} object.
#'
#' @param x An object of class \code{experiment}.
#'
#' @return The path to the \code{.gaml} file that contains the model definition.
#'
#' @examples
#' exp1 <- load_experiment("sir", system.file("examples", "sir.gaml",
#'                         package = "rama"))
#' model(exp1)
#'
#' @export
#'
model <- function(x) UseMethod("model")

#' @rdname model
#' @export
model.default <- function(x) "Unknown class"

#' @rdname model
#' @export
model.experiment <- function(x) attributes(x)$model
