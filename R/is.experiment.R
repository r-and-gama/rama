#' Tests if an object of type \code{"experiment"} is valid to pass on to gama.
#'
#' @param exp object to be tested
#'
#' @return The function returns `TRUE` or `FALSE` depending on whether its
#' argument is of chatacter type or not
#'
#' @examples
#' # to test if an object is a class `experiment`
#' \dontrun{
#' df <- data.frame("S0" = rep(999, 5), "I0" = rep(1, 5), "R0" = rep(0, 5),
#'                 "beta" = rep(1.5, 5), "gama" = runif (5, 0, 1),
#'                 "S" = rep(NA, 5), "I" = rep(1, 5), "R" = rep(1, 5),
#'                 "a" = rep(1000, 5), "b" = rep(1, 5))
#' exp <- experiment(parameters = c("S0", "I0", "R0", "beta", "gama"),
#'                   obsrates = c("S", "I", "R"),
#'                   tmax = "a",
#'                   seed = "b",
#'                   experiment = "sir",
#'                   model =
#'                     system.file("examples", "sir.gaml", package = "rama"),
#'                   df = df)
#' # expected an error because the input contains "NA"
#' is.experiment(exp)
#' }
#' @rdname experiment
#' @export
is.experiment <- function(exp) {

  if (any(is.na(exp)))
    stop("An object `experiment` cannot contain NA value.")
  if (any(is.null(exp)))
    stop("An object `experiment` cannot contain NULL value.")

  attr <- setdiff(c("class", "model", "experiment", "wkdir", "dic_g2r",
                    "dic_r2g"),
                  names(attributes(exp)))
  class <- setdiff(class(exp), c("experiment", "tbl_df", "tbl", "data.frame"))
  length(c(attr, class)) == 0
}
