#' Test if object is experiment
#'
#' Tests for objects of type \code{"experiment"}.
#'
#' @param exp object to be tested
#'
#' @return The function returns `TRUE` or `FALSE` depending on whether its
#' argument is of chatacter type or not
#'
#' @examples
#' is.experiment(exp)
#' @rdname experiment
#' @export
is.experiment <- function(exp) {

  if (any(is.na(exp))) stop("An object `experiment` can not contain NA value.")
  attr <- setdiff(c("class", "model", "experiment", "wkdir", "dic", "dic_rev"),
                  names(attributes(exp)))
  class <- setdiff(class(exp), c("data.frame", "experiment"))
  length(c(attr, class)) == 0
}
