#' Converting a data frame into an experiment
#'
#' Converts a data frame into an experiment, using the columns of the data
#' frame to populate values of parameters, periods of observation, duration of
#' simulation and seeds of an object of class experiment.
#'
#' @param df A data frame containing values of paramters, periods of observation,
#'           durations of simulation and seeds for an experiment.
#' @param parameters Names or indexes of columns of \code{df} corresponding to
#'                   values of parameters of the model specified in the
#'                   \code{model} \code{GAML} file. If unspecified, all the
#'                   columns with a name starting with "p_" will be considered.
#' @param obsrates Names or indexes of columns of \code{df} corresponding to
#'                 periods of observation for variables of the model specified
#'                 in the \code{model} \code{GAML} file. If unspecified, all
#'                 the columns with a name starting with "r_" will be considered.
#' @param tmax Name or index of the column of \code{df} that corresponds to the
#'             durations of the simulations. If not specified, the column named
#'             \code{tmax} will be considered.
#' @param seed Name or index of the column of \code{df} that corresponds to the
#'             seeds of the simulations. If not specified, the column named
#'             \code{seed} will be considered.
#' @inheritParams experiment
#'
#' @return An object of class \code{experiment}.
#'
#' @export
#'
#' @examples
#' # A first example:
#' rm(sir1)
#' df <- as.data.frame(repl(sir1, 5))
#' exp_name <- name(sir1)
#' gaml_file <- model(sir1)
#' as_experiment(df, experiment = exp_name, model = gaml_file)
#'
#' # Alternative uses:
#' as_experiment(df, 1:5, 6:8, 9, 10, exp_name, gaml_file)
#' as_experiment(df, c("p_S0", "p_I0", "p_R0", "p_beta", "p_gamma"),
#'               c("r_S", "r_I", "r_R"), "tmax", "seed", exp_name, gaml_file)
#'
#' # Or a mixture of character and numeric indexes:
#' as_experiment(df, 1:5, c("r_S", "r_I", "r_R"), "tmax", "seed", exp_name, gaml_file)
#'
#' # And even using default parameters specification:
#' as_experiment(df, obsrates = c("r_S", "r_I", "r_R"),
#'               experiment = exp_name, model = gaml_file)
#'
as_experiment <- function(df, parameters = NULL, obsrates = NULL, tmax = "tmax",
                          seed = "seed", experiment, model, dir = "", dic = NULL) {

  if (missing(experiment))
    stop ("A name of experiment should be provided for argument 'experiment'.")

  if (missing(model))
    stop ("A path to a GAML file should be provided for argument 'model'.")

  if (is.null(parameters)) {
    parameters <- df[, grep("^p_", names(df))]
  } else {
    parameters <- df[, parameters]
  }
  names(parameters) <- sub("^p_", "", names(parameters))

  if (is.null(obsrates)) {
    obsrates <- df[, grep("^r_", names(df))]
  } else {
    obsrates <- df[, obsrates]
  }
  names(obsrates) <- sub("^r_", "", names(obsrates))

  experiment(parameters, obsrates, df[, tmax], df[, seed],
             experiment, model, dir, dic)
}
