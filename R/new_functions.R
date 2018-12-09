# get_parameters ---------------------------------------------------------------

template_get <- function(x, slot, value) {
  x2 <- do.call(rbind, x[[slot]])
  x3 <- do.call(data.frame, as.list(as.numeric(x2[, value])))
  setNames(x3, x2[, "name"])
}




get_parameters <- function(...) template_get(..., "Parameters", "value")




get_variables <- function(...) template_get(..., "Outputs", "framerate")




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
