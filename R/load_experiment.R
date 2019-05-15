# get_parameters ---------------------------------------------------------------
get_parameters <- function(x) {
  parameters <- x[["Parameters"]]
  if (is.null(parameters)) return(NULL)
  x2 <- do.call(rbind, parameters)
  x3 <- do.call(data.frame, as.list(as.numeric(x2[, "value"])))
  x3 <- setNames(x3, x2[, "name"])
  sel <- grep("INT", x2[, "type"])
  if (length(sel) > 0) x3[, sel] <- lapply(x3[, sel], as.integer)
  x3
}



# get_variables ----------------------------------------------------------------
get_variables <- function(x) {
  outputs <- x[["Outputs"]]
  if (is.null(outputs)) return(NULL)
  x2 <- do.call(rbind, outputs)
  x3 <- do.call(data.frame, as.list(as.numeric(x2[, "framerate"])))
  x3 <- setNames(x3, x2[, "name"])
  x3[] <- lapply(x3, as.integer) # frame rates are necessarily integers.
  x3
}



# get_attributes ---------------------------------------------------------------
#' @importFrom stats setNames
get_attributes <- function(x) {
  out <- setNames(do.call(function(...)
    data.frame(..., stringsAsFactors = FALSE),
    as.list(x$.attrs[c("finalStep", "seed", "sourcePath", "experiment")])),
    c("tmax", "seed", "gaml", "experiment"))
  out$tmax <- as.integer(out$tmax)
  out$seed <- as.numeric(out$seed)
  out
}



# Check the consistency between gaml and experiment object `exp` created in rama
check_param_type <- function(exp, model) {
# pending
}



# load_experiment --------------------------------------------------------------
#' Load an experiment from a GAML file
#'
#' Loads an experiment from a \code{.gaml} file and returns an object of class
#' \code{experiment}.
#'
#' The \code{rama} package contains an internal collection of GAMA models. These
#' models are specified, at minima, by a \code{.gaml}. Additional files such as
#' shapefile can be used to specify a model, in which case they are in the same
#' directory as the \code{.gaml} file. The internal collection of GAMA models is
#' in the \code{examples} directory of the \code{rama} package file hierarchy.
#' These models can be accessed with the \code{\link[base]{system.file}}
#' function as explained in the example.
#'
#' @param exp The name of the experiment to load.
#' @param model The name of the GAML file from which to load the experiment.
#' @example inst/examples/load_experiment.R
#' @importFrom XML xmlToList xmlParse
#'
#' @export
load_experiment <- function(exp, model) {

  # Reading GAML file:
  message(cat("Loading experiment \"", exp,
                 "\" from file \"", basename(model), "\"...", sep = ""))
  out <- read_gaml_experiment(exp, model)

  # Check if experiment and type requested are valid:
  check_experiment(exp, list("info" = out))

  # Retrieving information:
  make_df_dic <- function(x) {
    if (is.null(x)) return(list(out = data.frame(NULL), dic = NULL))
    the_names <- names(x)
    dic_g2r <- make_dictionary(the_names)
    names(x) <- dic_g2r
    list(out = x, dic_g2r = dic_g2r)
  }
  variables <- make_df_dic(get_variables(out))
  parameters <- make_df_dic(get_parameters(out))
  out_attr <- get_attributes(out)

  # Returning experiment object:
  experiment(parameters$out, variables$out, out_attr$tmax, out_attr$seed,
             exp, model, c(parameters$dic_g2r, variables$dic_g2r))
}
