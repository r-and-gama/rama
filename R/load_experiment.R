# get_parameters ---------------------------------------------------------------
get_parameters <- function(x) {
  x2 <- do.call(rbind, x[["Parameters"]])
  x3 <- do.call(data.frame, as.list(as.numeric(x2[, "value"])))
  x3 <- setNames(x3, x2[, "name"])
  sel <- grep("INT", x2[, "type"])
  if (length(sel) > 0) x3[, sel] <- lapply(x3[, sel], as.integer)
  x3
}

# get_variables ----------------------------------------------------------------
get_variables <- function(x) {
  x2 <- do.call(rbind, x[["Outputs"]])
  x3 <- do.call(data.frame, as.list(as.numeric(x2[, "framerate"])))
  x3 <- setNames(x3, x2[, "name"])
  x3[] <- lapply(x3, as.integer)
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

# Check if a requested experiment is valid: name exists and type = gui ---------
check_experiment <- function(exp, model) {
  exp_info <- show_experiment(model)
  # check if experiment requested is declared in gaml
  if (!exp %in% exp_info$experiment)
    stop(paste0("There is no experiment named \"", exp, "\" in ",
                basename(model)))
  # check if experiment requested has valid type
  type <- exp_info$type[exp_info$experiment == exp]
  if (type != "gui")
    stop(paste0("Experiment \"", exp, "\" of type \"", type,
                "\" is not supported."))
  invisible(0)
}

# load_experiment --------------------------------------------------------------
#' Load an experiment from a model
#'
#' Loads an experiment from a model specified in a \code{.gaml} file and returns
#' an object of class \code{experiment}.
#'
#' The \code{rama} package contains a library of model in the \code{examples}
#' directory of the \code{rama} package file hierarchy. These models can be
#' accessed with the \code{system.file()} function as explained in the example.
#'
#' @param exp The name of the experiment to load.
#' @param model The name of the file from which to load the experiment.
#' @param dir The name of the directory to save the output of the runs for each
#' model. If not specified, name of the gaml file will be used
#'
#' @examples
#' # Looking at the \code{sir.gaml} file in the \code{examples} directory of the
#' # \code{rama} library:
#' # Loading an experiment:
#' exp1 <- load_experiment("sir", system.file("examples", "sir.gaml", package = "rama"))
#'
#' # Checking the class:
#' class(exp1)
#'
#' @importFrom XML xmlToList xmlParse
#'
#' @export
load_experiment <- function(exp, model, dir = "") {

  # Check if experiment and type requested are valid
  check_experiment(exp, model)

  # Make working directory
  wk_dir <- make_wkdir(dir, model)

  # Loading experiment
  message(cat("Loading experiment \"", exp,
              "\" from file \"", basename(model), "\"...", sep = ""))
  tmp <- tempfile(fileext = ".xml")
  system(paste0("java -jar ", getOption("rama.startjar"),
                " -Xms", getOption("rama.Xms"),
                " -Xmx", getOption("rama.Xmx"),
                " -Djava.awt.headless=true org.eclipse.core.launcher.Main",
                " -application msi.gama.headless.id4 -xml ",
                exp, " '", model, "' ", tmp, " > /dev/null"),
         ignore.stdout = TRUE, ignore.stderr = TRUE)
  unlink("workspace", TRUE, TRUE)

  if (file.exists(tmp)) {
    out <- XML::xmlToList(XML::xmlParse(tmp))
  } else {
    stop(paste0("Gama fails to read your experiment"))
  }

  out <- out$Simulation
  if (!is.null(out$Outputs)) {
    out_var <- get_variables(out)
    dicar <- make_dictionary(names(out_var))
    names(out_var) <- paste0("r_", dicar[names(out_var)])

  } else {
    out_var <- data.frame(NULL)
    dicar <- NULL
  }
  if (!is.null(out$Parameters)) {
    out_par <- get_parameters(out)
    dic_par <- make_dictionary(names(out_par))
    names(out_par) <- paste0("p_", dic_par[names(out_par)])
  } else {
    out_par <- data.frame(NULL)
    dic_par <- NULL
  }

  dic <- c(dic_par, dicar)
  test_schar(names(dic))

  out_attr <- get_attributes(out)
  output <- as.data.frame(c(out_par, out_var, out_attr))
  output$gaml <- NULL
  output$experiment <- NULL
  class(output) <- c("experiment", class(output))
  attr(output, "model") <- as.character(unname(out_attr$gaml))
  attr(output, "experiment") <- as.character(unname(out_attr$experiment))
  attr(output, "wkdir") <- wk_dir
  attr(output, "dic") <- dic
  attr(output, "dic_rev") <- setNames(names(dic), dic)

  output
}
