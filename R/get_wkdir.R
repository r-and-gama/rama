# get_wkdir --------------------------------------------------------------------
#' Get the output directory
#'
#' These functions allow to get and set the path to the directory where the
#' simulations outputs will be saved.
#'
#' @param x An object of class \code{experiment}.
#'
#' @return The path of the directory where the simulations outputs will be saved.
#'
#' @examples
#' exp1 <- load_experiment("sir", system.file("examples", "sir.gaml",
#'                         package = "rama"))
#' get_wkdir(exp1)
#'
#' @export
#'
get_wkdir <- function(x) UseMethod("get_wkdir")

#' @rdname get_wkdir
#' @export
get_wkdir.default <- function(x) "Unknown class"

#' @rdname get_wkdir
#' @export
get_wkdir.experiment <- function(x) attributes(x)$wkdir
