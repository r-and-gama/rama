# fill_experiment --------------------------------------------------------------
#' Add output of the ran experiment to the experiment object
#'
#' It puts
#'
#'
#' @param output An object of class \code{list}.
#' @param exp An object of class \code{experiment}.
#'
#' @return Returns an experiment that contains its results.
#'
#' @example inst/examples/fill_experiment.R
#' @export
fill_experiment <- function(output, exp) {
  if (!(is.list(output) & length(output) == nrow(exp))) {
     for (j in (1:length(output))) {
       # exp[j,"RUN"] <- TRUE
       # exp[j,"RES"] <- output[[j]]
     }
  }
  return(output)
}
