# get_parameters ---------------------------------------------------------------

template_get <- function(x, slot, value) {
  x2 <- do.call(rbind, x[[slot]])
  x3 <- do.call(data.frame, as.list(as.numeric(x2[, value])))
  setNames(x3, x2[, "name"])
}

get_parameters <- function(...) template_get(..., "Parameters", "value")

get_variables <- function(...) template_get(..., "Outputs", "framerate")

#' @importFrom utils setNames
get_attributes <- function(x) {
  setNames(do.call(data.frame,
                   as.list(x$.attrs[c("finalStep", "seed", "sourcePath")])),
           c("tmax", "seed", "gaml"))
}

# load_experiment --------------------------------------------------------------

#' Load An Experiment
#'
#' Loads an experiment from a model specified in a \code{gaml} file and returns
#' an object of class \code{plan}.
#'
#' @param experiment The name of the experiment to load.
#' @param model The name of the file from which to load the experiment.
#'
#' @importFrom XML xmlToList xmlParse
#'
#' @export
load_experiment <- function(experiment, model) {
  message("Loading experiment '", experiment,
          "' from file '", basename(model), "'...")
  tmp <- tempfile(fileext = ".xml")
  system(paste0("java -jar ", getOption("rama.startjar"),
                " -Xms", getOption("rama.Xms"),
                " -Xmx", getOption("rama.Xmx"),
                " -Djava.awt.headless=true org.eclipse.core.launcher.Main",
                " -application msi.gama.headless.id4 -xml ",
                experiment, " ", model, " ", tmp, " > /dev/null"),
         ignore.stdout = TRUE, ignore.stderr = TRUE)
  unlink("workspace", TRUE, TRUE) # removes the above-created workspace directory
  out <- xmlToList(xmlParse(tmp))$Simulation
  out <- lapply(c(get_parameters, get_variables, get_attributes), function(f) f(out))
  the_names <- lapply(out, names)
  out <- do.call(cbind, out)
  class(out) <- c("experiment", class(out))
  attr(out, "model") <- as.character(unname(out$gaml))
  attr(out, "parameters") <- the_names[[1]]
  attr(out, "variables") <- the_names[[2]]
  attr(out, "tmax") <- the_names[[3]][1]
  attr(out, "seed") <- the_names[[3]][2]
  out$gaml <- NULL
  out
}

#' @export
model <- function(x) UseMethod("model")

#' @export
model.default <- function(x) "Unknown class"

#' @export
model.experiment <- function(x) {
  attributes(x)$model
}


#' @export
parameters <- function(x) UseMethod("parameters")

#' @export
parameters.default <- function(x) "Unknown class"

#' @export
parameters.experiment <- function(x) {
  x[, attributes(x)$parameters]
}


#' @export
variables <- function(x) UseMethod("variables")

#' @export
variables.default <- function(x) "Unknown class"

#' @export
variables.experiment <- function(x) {
  x[, attributes(x)$variables]
}

#' @export
#'
#' @examples
#' my_experiment <- experiment(
#'   expand.grid(S0 = c(900, 950, 999),
#'               I0 = c(100, 50, 1),
#'               R0 = 0,
#'               beta = 1.5,
#'               gamma = .15),
#'   data.frame(S = 1, I = 1, R = 1),
#'   tmax = 1000,
#'   seed = 1,
#'   model = "/Users/choisy/Dropbox/aaa/r-and-gama/rama/inst/examples/sir.gaml"
#' )
experiment <- function(parameters, outputs, tmax, seed, model) {
  out <- cbind(param_val, output_val, tmax, seed)
  attr(out, "model") <- model
  attr(out, "parameters") <- names(parameters)
  attr(out, "variables") <- names(outputs)
  attr(out, "tmax") <- "tmax"
  attr(out, "seed") <- "seed"
  class(out) <- c("experiment", "data.frame")
  out
}
