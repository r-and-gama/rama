# constructor ------------------------------------------------------------------
new_experiment <- function(parameters, obsrates, tmax, seed, experiment, model,
                           dir = "", dic_g2r = NULL) {

# This function automatically adds "p_" and "r_" prefixes to the parameteres and
# observation rates. It also does so to the dictionary if provided.


  stopifnot(is.data.frame(parameters))
  stopifnot(is.data.frame(obsrates))
  stopifnot(nrow(parameters) == nrow(obsrates))
  stopifnot(is.numeric(tmax)) # has to be an integer
  stopifnot(is.character(experiment))
  stopifnot(is.character(model))
  stopifnot(is.character(dir))
  stopifnot(is.character(dic_g2r))
  stopifnot(is.character(names(dic_g2r)))

  names_param <- names(parameters)
  names_obsrates <- names(obsrates)
  oldnames <- c(names_param, names_obsrates)
  newnames <- c(paste0("p_", names_param), paste0("r_", names_obsrates))

  if (is.null(dic_g2r)) {
    dic_g2r <- setNames(newnames, oldnames)
  } else {
    stopifnot(all(dic_g2r %in% oldnames))
    dic_g2r <- c(setNames(paste0("p_", dic_g2r[which(dic_g2r %in% names_param)]),
                          names(dic_g2r[which(dic_g2r %in% names_param)])),
                 setNames(paste0("r_", dic_g2r[which(dic_g2r %in% names_obsrates)]),
                          names(dic_g2r[which(dic_g2r %in% names_obsrates)])))
  }

  obsrates[] <- lapply(obsrates, as.integer)
  structure(setNames(cbind(parameters,
                           obsrates,
                           tmax = as.integer(tmax),
                           seed = seed), c(newnames, "tmax", "seed")),
            class      = c("experiment", "tbl_df", "tbl", "data.frame"),
            model      = model,
            experiment = experiment,
            wkdir      = make_wkdir(model, dir),
            dic_g2r    = dic_g2r,
            dic_r2g    = setNames(names(dic_g2r), dic_g2r))
}



# validator --------------------------------------------------------------------
validate_experiment <- function(x) {
  model <- model(x)
  dic_g2r <- attr(x, "dic_g2r")
  dic_r2g <- attr(x, "dic_r2g")
  colnames <- lapply(c(parameters, obs_rates), function(f) names(f(x)))

  check_experiment(name(x), model)
  test_schar(names(dic_g2r))

  if (any(obs_rates(x) < 0))
    stop("The period of observation should be positive integers.")

  if (any(x$tmax < 0))
    stop("The end steps of simulations should be positive integers.")

  if (length(setdiff(unlist(colnames), dic_g2r)) > 0)
    stop("Some variables or parameters names are not in the dictionary.")

  if (setequal(dic_g2r, names(dic_r2g)) + setequal(names(dic_g2r), dic_r2g) < 2)
    stop("The dictionaries are inconsistent.")

  diff <- setdiff(dic_r2g[colnames[[1]]], get_parameters_names(model))
  if (length(diff) > 1) {
    stop(paste0("The parameters names '", substitute(diff),
               "' do not correspond to any parameter in the '",
               basename(model), "' file."))
  } else if (length(diff) > 0) {
    stop(paste0("The parameter name '", substitute(diff),
               "' does not correspond to any parameter in the '",
               basename(model), "' file."))
  }

  diff <- setdiff(dic_r2g[colnames[[2]]], get_variables_names(model))
  if (length(diff) > 1) {
    stop(paste0("The variables names '", substitute(diff),
               "' do not correspond to any variable in the '",
               basename(model), "' file."))
  } else if (length(diff) > 0) {
    stop(paste0("The variable name '", substitute(diff),
               "' does not correspond to any variable in the '",
               basename(model), "' file."))
  }

  x
}



# helper -----------------------------------------------------------------------
#' Constructor of experiments
#'
#' Allows to build an object of class \code{experiment} from individual parts.
#'
#' The class \code{experiment} inherits from the class \code{tibble}
#' (\code{tbl_df}). It contains parameters values as well as periods of
#' observation of the observed variables and it connects to a \code{GAML} file
#' that specifies the full model as well as to a directory that contains the
#' outputs of simuations.
#'
#' TO DO: Explains how \code{dir} is made is not specified. Explains what
#' \code{dic} is.
#'
#' @param parameters A data frame of numerical values giving the values of each
#'                   parameter (in column), for each simulation (in row).
#' @param obsrates A data frame of positive integer values giving the periods,
#'                 in time steps, at which observed variables are observed.
#'                 Should have the same number of rows as \code{parameters}.
#' @param tmax A positive integer vector, the length of which is either 1 or
#'             equal to the number of \code{parameters} and \code{obsrates}: it
#'             gives the end of simulations, in numbers of time steps.
#' @param seed A numerical vector, the length of which is either 1 or equal to
#'             the number of \code{parameters} and \code{obsrates}: it gives the
#'             seeds of the simulations.
#' @param experiment The name of an experiment of the \code{GAML} file
#'                   \code{model}.
#' @param model The path to a \code{GAML} file.
#' @param dir The path to a directory where the simulations output are saved. If
#'            not specified (i.e. empty character string, default), then the
#'            simulations are saved in a subdirectory of the working directory,
#'            the name of which is made from the name of \code{model}. See
#'            \code{Details} for more information.
#' @param dic A named vector of character strings. The values and the names of
#'            this vector should be consistent with the names of
#'            \code{parameters}, \code{obsrates} as well as the variables and
#'            parameters defined in the \code{model} \code{GAML} file. See
#'            Details for more information.
#'
#' @return An object of class \code{experiment}.
#'
#' @export
#'
#' @example inst/examples/experiment.R
#'
#'
experiment <- function(parameters, obsrates, tmax, seed, experiment, model,
                       dir = "", dic = NULL) {
  validate_experiment(new_experiment(parameters, obsrates, tmax, seed,
                                     experiment, model, dir, dic))
}
