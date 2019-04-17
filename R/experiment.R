# constructor ------------------------------------------------------------------
#' @importFrom tools md5sum
#' @importFrom purrr map2
#' @import dplyr

new_experiment <- function(parameters, obsrates, tmax, seed,
                           experiment, model, dic_g2r = NULL, output = NA) {

# Automatically adds "p_" and "r_" prefixes to the parameteres and observation
# rates. It also does so to the dictionary if provided.

# Automatically converts periods of obsrates and tmax into integer if there are
# not.

# dic_g2r: contains the new names of the parameters and variables and the names
# of this vector contains the old names. It is the opposite for dic_r2g.

# exp0 <- rama:::new_experiment(
#   data.frame(S0 = 999, I0 = 1, R0 = 0, beta = 1.5, gamma = .15),
#   data.frame(S = 1, I = 1, R = 1),
#   1000, 1, "sir", system.file("models", "sir.gaml", package = "rama"))

  stopifnot(is.data.frame(parameters))
  stopifnot(is.data.frame(obsrates))
  stopifnot(nrow(parameters) == nrow(obsrates))
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
  if (!is.na(output)) {
    if (nrow(parameters) > 1) {
      stopifnot(is.list(output) &
                length(output) > 1 &
                all(sapply(output, is.data.frame)))
    } else {
      stopifnot(is.data.frame(output))
    }
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

  obsrates[] <- lapply(obsrates, as.integer)

  model_info <- list("path" = model,
                     "info" = read_gaml_experiment(experiment, model),
                     "md5sum" = md5sum(model))
  out <- structure(setNames(cbind(parameters,
                           obsrates,
                           tmax = as.integer(tmax),
                           seed = seed,
                           output = output), c(newnames, "tmax", "seed", "output")),
            class      = c("experiment", "tbl_df", "tbl", "data.frame"),
            model      = model_info,
            experiment = experiment,
            dic_g2r    = dic_g2r,
            dic_r2g    = setNames(names(dic_g2r), dic_g2r))
 # cast parameter types
  types <- map_type(get_info(out, "Parameters", "type"))
  functions <- lapply(paste0("as.", types), function(x) match.fun(x))
  mapply(function(n, f) {
    out[, n] <<- f(out[, n][[1]])
    invisible()
  }, names(types), functions)

  # cast observation rate types
  old_attr <- attributes(out)
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  out_r <- out[, grepl("r_", names(out))]
  out_r[] <- lapply(out_r, as.integer)
  out[, grepl("r_", names(out))] <- out_r
  attributes(out) <- old_attr
  out
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

  diff <- setdiff(dic_r2g[colnames[[1]]], get_info(x, "Parameters", "name"))
  if (length(diff) > 1) {
    stop(paste0("The parameters names '", substitute(diff),
               "' do not correspond to any parameter in the '",
               basename(model$path), "' file."))
  } else if (length(diff) > 0) {
    stop(paste0("The parameter name '", substitute(diff),
               "' does not correspond to any parameter in the '",
               basename(model$path), "' file."))
  }

  diff <- setdiff(dic_r2g[colnames[[2]]], get_info(x, "Outputs", "name"))
  if (length(diff) > 1) {
    stop(paste0("The variables names '", substitute(diff),
               "' do not correspond to any variable in the '",
               basename(model), "' file."))
  } else if (length(diff) > 0) {
    stop(paste0("The variable name '", substitute(diff),
               "' does not correspond to any variable in the '",
               basename(model$path), "' file."))
  }
  # check parameter types
  type_r <- sapply(parameters(x), class)
  type_g <- map_type(get_info(x, "Parameters", "type"))
  diff <- type_r == type_g
  if (!all(diff)) {
    stop(paste0("The data type(s) of '",
                paste(names(type_g)[diff], collapse = ", "),
                "' do not correspond to parameter type(s) declared in the '",
                basename(model$path), "' file."))
  }
  # check obs_rates

  if (!all(sapply(obs_rates(x), is.integer))) {
    stop(paste0("The observation rates must be interger as declared in '",
                basename(model$path), "' file."))
  }

  # validate snapshot
  current_md5sum <-  md5sum(model(x)$path)

  if(current_md5sum != model$md5sum)
    stop(paste0("Gaml file '", model$path, "' has been changed.
                Please use function 'model<-' to add this gaml file
                to the experiment"))

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
#' that specifies the full model.
#'
#' TO DO: Explains what \code{dic} is.
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
experiment <- function(parameters, obsrates, tmax, seed, experiment, model,
                       dic = NULL) {
  validate_experiment(new_experiment(parameters, obsrates, tmax, seed,
                                     experiment, model, dic))
}
