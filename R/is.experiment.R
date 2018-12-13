#' Test if object is experiment
#'
#' Tests for objects of type \code{"experiment"}.
#'
#' @param x object to be tested
#'
#' @return The function returns `TRUE` or `FALSE` depending on whether its
#' argument is of chatacter type or not
#'
#' @examples
#' gaml_file <- system.file("examples", "sir.gaml", package = "rama")
#' exp1 <- load_experiment("sir", gaml_file, "sir")
#'
#' is.experiment(exp1)
#' @export
is.experiment <- function(x) {

  if (any(is.na(x))) stop("An object `experiment` can not contain NA value.")
  attr <- setdiff(c("class", "model", "experiment", "wkdir", "dic", "dic_rev"),
                  names(attributes(x)))
  class <- setdiff(class(x), c("data.frame", "experiment"))
  length(c(attr, class)) == 0
}
