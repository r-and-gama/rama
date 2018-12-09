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
  return(out)
  do.call(plan, lapply(list(get_parameters,
                            get_seed,
                            get_duration,
                            get_observation,
                            get_process),
                       function(f) f(out)))
}


