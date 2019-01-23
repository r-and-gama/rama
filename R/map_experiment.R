#' Map a data frame on an experiment
#'
#' Maps the data of a data frame on the structure on an object of class
#' \code{experiment}.
#'
#' The names of the columns of the data frame should correspond to the names of
#' the variables and parameters of the experiment. This can work only if one of
#' the following conditions is met:
#' \itemize{
#'   \item the sets of names of the data frame and experiment are identical;
#'   \item the names of the data frame are included in the names of the
#'         experiment AND the numbers of rows of the data frame and the
#'         experiment are identical.
#' }
#'
#' @param df A data frame containing variables and parameters values for a
#'           number of simulations we wish to use for the experiment \code{exp}.
#' @param exp An object of class \code{experiment}. See \code{Details} for
#'            constraints on this object.
#'
#' @export
#'
#' @examples
#' # First situation: the sets of names of the data frame and the experiment
#' # are exactly the same:
#' rm(sir1)
#' exp <- sir1
#' df <- as.data.frame(repl(sir1, 3))
#' map_experiment(df, exp)
#'
#' # Second situation: the names of the data frame are included in the names of
#' # the experiment AND the numbers of rows of the data frame and the experiment
#' # are equal:
#' exp <- repl(sir1, 3)
#' df <- as.data.frame(exp)[, c(1, 3, 6)]
#' df[] <- 2
#' map_experiment(df, exp)
#'
#'
map_experiment <- function(df, exp) {
  stopifnot(is.data.frame(df))

  if (setequal(names(df), names(exp))) {
    the_attributes <- attributes(exp)
    the_attributes$row.names <- row.names(df)
    attributes(df) <- the_attributes
    return(validate_experiment(df))
  }

  if (all(names(df) %in% names(exp)) & nrow(df) == nrow(exp)) {
      exp[, names(df)] <- df
      return(validate_experiment(exp))
  }

  stop(cat("A data frame 'df' can be mapped on an experiment 'exp' only\n",
           "if one of the following conditions is met:\n",
           "  - the sets of names of 'df' and 'exp' are identical;",
           "  - the names of 'df' are included in the names of 'exp'",
           "    AND the numbers of rows of 'df' and 'exp' are identical."))
}
