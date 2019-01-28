# model ------------------------------------------------------------------------
#' Get/Set the model of an experiment
#'
#' These functions allow to get and set the path to the \code{.gaml} file that
#' contains the model of an \code{experiment} object.
#'
#' @param exp An object of class \code{experiment}.
#'
#' @return The path to the \code{.gaml} file that contains the model definition.
#'
#' @example inst/examples/model.R
#' @export
#'
model <- function(exp) UseMethod("model")

#' @rdname model
#' @export
model.default <- function(exp) "Unknown class"

#' @rdname model
#' @export
model.experiment <- function(exp) attributes(exp)$model

# set_model---------------------------------------------------------------------
#' This function allows to change the model path of an experiment object
#'
#' @param value Path of new gaml model file
#'
#' @rdname model
#' @example inst/examples/model.R
#' @export
`model<-` <- function(exp, value) UseMethod("model<-")

#' @rdname model
#' @export
`model<-.default` <- function(exp, value) "Unknown class"

#' @rdname model
#' @importFrom utils capture.output
#' @export
`model<-.experiment` <- function(exp, value){
  # check if experiment name and type are valid in the requested model
  check_experiment(name(exp), value)
  # check if model parameters and observed parameters correspond
  invisible(capture.output(tmp <- load_experiment(name(exp),
                                                  value,
                                                  dir = tempfile(c("abcd")))))

  if (all(parameters(exp) == parameters(tmp)) &
     all(obs_rates(exp) == obs_rates(tmp)) &
     any(names(exp) == "tmax") &
     any(names(exp) == "seed"))
    attr(exp, "model") <- value
  else
    stop(paste0("Either Parameters or observation rates or tmax or seed in \"",
                exp, "\" doesn't match with the requested model \"",
                value, "\""))
  return(exp)
}
