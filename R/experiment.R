# constructor ------------------------------------------------------------------
#' Contructor of experiment class
#'
#' Builds up an object of class experiment from scratch
#'
#' The philosophy of the constructor is to contains the minimal number of checks
#' required for the structure of the object to be sound. All the other checks
#' should be in the validator below. The constructor automatically adds "p_" and
#' "r_" prefixes to the parameteres and observation rates. It also does so to
#' the dictionary if provided. It also automatically converts periods of
#' obsrates and tmax into integer if there are not.
#'
#' @param dic_g2r named character vector containing the new names of the
#' parameters and variables and the names of this vector contains the old names.
#' It is the opposite for \code{dic_r2g}.
#'
#' @noRd
#' @examples
#' exp0 <- rama:::new_experiment(
#'   data.frame(S0 = 999, I0 = 1, R0 = 0, beta = 1.5, gamma = .15),
#'   data.frame(S = 1, I = 1, R = 1),
#'   1000, 1, "sir", system.file("models", "sir.gaml", package = "rama"))
#'
new_experiment <- function(parameters, obsrates, tmax, seed, experiment, model,
                           dic_g2r = NULL, tseries = NA, images = NA) {

  #  nrp <- nrow(parameters)

  stopifnot(is.data.frame(parameters))
  stopifnot(is.data.frame(obsrates))
  stopifnot(is.numeric(tmax))
  stopifnot(is.null(dim(seed)))
  stopifnot(is.character(experiment))
  stopifnot(length(experiment) == 1)
  stopifnot(is.character(model))
  stopifnot(length(model) == 1)

  if (!is.null(dic_g2r)) {
    stopifnot(is.character(dic_g2r))
    stopifnot(is.character(names(dic_g2r)))
  }
  nobs <- ncol(obsrates)
  stopifnot(length(dic_g2r) >= ncol(parameters) + nobs)

  test <- !all(is.na(tseries))
  if (test) stopifnot(is.list(tseries))
  if (!is.na(images)) stopifnot(is.character(images))

  nb <- c(nrow(parameters), nrow(obsrates),
          length(tmax), length(seed),
          length(tseries), length(images))
  stopifnot(all(nb > 0))
  stopifnot(length(unique(nb[nb > 1])) < 2)

  if (test) {
    tseries <- tseries[!is.na(tseries)]
    stopifnot(all(sapply(tseries, is.data.frame)))
    stopifnot(all(sapply(tseries, ncol) == nobs))
    stopifnot(all(sapply(tseries, nrow) >= sapply(mapply(seq, 1, tmax, apply(obsrates, 1, min), SIMPLIFY = FALSE), length)))
  }

# Generating new names:
  names_param <- names(parameters)
  names_obsrates <- names(obsrates)
  oldnames <- c(names_param, names_obsrates)
  newnames <- c(paste0("p_", names_param), paste0("r_", names_obsrates))

# Dealing with dictionaries:
  if (is.null(dic_g2r)) {
    dic_g2r <- setNames(newnames, oldnames)
  } else {
    stopifnot(all(dic_g2r %in% oldnames))
    sel1 <- which(dic_g2r %in% names_param)
    sel2 <- which(dic_g2r %in% names_obsrates)
    dic_g2r <- c(setNames(paste0("p_", dic_g2r[sel1]), names(dic_g2r[sel1])),
                 setNames(paste0("r_", dic_g2r[sel2]), names(dic_g2r[sel2])))
  }

# Dealing with obsrates and tmax, converting them into integers if needed:
  if (any(!sapply(obsrates, is.integer))) {
    message(cat("Periods of observation (\"obsrates\") are rounded and converted into integers."))
    obsrates[] <- lapply(obsrates, function(x) as.integer(round(x)))
  }
  if (!is.integer(tmax)) {
    message(cat("Final time step (\"tmax\") is rounded and converted into integer."))
    tmax <- as.integer(tmax)
  }

# returning the object:
  structure(setNames(cbind(parameters,
                           obsrates,
                           tmax = as.integer(tmax),
                           seed = seed,
                           tseries = tseries,
                           images = images),
                     c(newnames, "tmax", "seed", "tseries", "images")),
            class      = c("experiment", "tbl_df", "tbl", "data.frame"),
            model      = model,
            experiment = experiment,
            dic_g2r    = dic_g2r,
            dic_r2g    = setNames(names(dic_g2r), dic_g2r))
}





# validator --------------------------------------------------------------------
validate_experiment <- function(x) {
# TO DO: #######################################################################
#   parameters: we should check that the types of the parameters are the same as
#               in the .gaml file.
#   seed: we should check that the type of the seed are the same as in the .gaml
#         file.
################################################################################

# periods of observations should be strictly positive integers
  or <- obs_rates(x)
  if (any(!sapply(or, is.integer)))
    stop("The periods of observations should be integers.")
  if (any(or < 1))
    stop("The periods of observation should be strictly positive integers.")

# tmax should be a stricly positive integer
  tm <- x$tmax
  if (any(!sapply(tm, is.integer)))
    stop("The final steps of simulations should be integers.")
  if (any(tm < 1))
    stop("The final steps of simulations should be strictly positive integers.")

# the names of the time series in the data frames of the "tseries" slot should
# be consistant with the names of the periods of observation.
  ts <- x$tseries
  if (!all(is.na(ts))) {
    ts <- ts[!is.na(ts)]
    if (any(!sapply(ts, is.data.frame)))
      stop("The elements of the \"tseries\" slot should be either NA or data frames.")

    the_names <- unique(lapply(ts, function(x) sort(names(x))))
    if (length(the_names) > 1)
      stop("The names of the variables in the data frames of the \"tseries\" slot should all be the same.")
    unlist(the_names)
  }

  model <- model(x)
  dic_g2r <- attr(x, "dic_g2r")
  dic_r2g <- attr(x, "dic_r2g")
  colnames <- lapply(c(parameters, obs_rates), function(f) names(f(x)))

  check_experiment(name(x), model)
  test_schar(names(dic_g2r))


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
