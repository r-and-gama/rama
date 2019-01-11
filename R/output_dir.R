# output_dir --------------------------------------------------------------------
#' Get the output directory
#'
#' These functions allow to get and set the path to the directory where the
#' simulations outputs will be saved.
#'
#' @param exp An object of class \code{experiment}.
#'
#' @return The path of the directory where the simulations outputs will be
#' saved.
#'
#' @examples
#' exp1 <- load_experiment("sir", system.file("examples", "sir.gaml",
#'                         package = "rama"))
#' output_dir(exp1)
#'
#' @export
#'
output_dir <- function(exp) UseMethod("output_dir")

#' @rdname output_dir
#' @export
output_dir.default <- function(exp) "Unknown class"

#' @rdname output_dir
#' @export
output_dir.experiment <- function(exp){
  dir <- attributes(exp)$wkdir
  if(!dir.exists(dir)) {
    stop(paste0("The directory '", dir, "' associated with the experiment '",
                name(exp), "' doesn't exist"))
  }
  dir
}
