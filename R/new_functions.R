# get_parameters ---------------------------------------------------------------

get_parameters <- function(x) {
  x2 <- do.call(rbind, x[["Parameters"]])
  x3 <- do.call(data.frame, as.list(as.numeric(x2[, "value"])))
  x3 <- setNames(x3, x2[, "name"])
  sel <- grep("INT", x2[, "type"])
  x3[, sel] <- lapply(x3[, sel], as.integer)
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
  out <- setNames(do.call(function(...) data.frame(..., stringsAsFactors = FALSE),
                          as.list(x$.attrs[c("finalStep", "seed", "sourcePath", "experiment")])),
                  c("tmax", "seed", "gaml", "experiment"))
  out$tmax <- as.integer(out$tmax)
  out$seed <- as.numeric(out$seed)
  out
}




# load_experiment --------------------------------------------------------------

#' Load an experiment from a model
#'
#' Loads an experiment from a model specified in a \code{gaml} file and returns
#' an object of class \code{experiment}.
#'
#' @param experiment The name of the experiment to load.
#' @param model The name of the file from which to load the experiment.
#' @param dir The name of the directory to save the output of the runs for each
#' model. If not specified, name of the gaml file will be used
#'
#' @examples
#' # Looking at the \code{sir.gaml} file in the \code{examples} directory of the
#' # \code{rama} library:
#' gaml_file <- system.file("examples", "sir.gaml", package = "rama")
#'
#' # Loading the experiment \code{sir} from this gaml file:
#' exp1 <- load_experiment("sir", gaml_file)
#'
#' # Checking the class:
#' class(exp1)
#'
#' @importFrom XML xmlToList xmlParse
#'
#' @export
load_experiment <- function(experiment, model, dir = "") {

  if (!file.exists(model)) {
    stop(paste0("There is no file \"", model, "\"."))
  }
  # Making working directory

  message(cat("Creating working directory \"", dir, "\" in \"", getwd(), "\".", sep = ""))

  if(dir == ""){
    # get model name from gaml file
    dir <- gsub(".gaml", "", basename(model))
    message(cat("The directory \"", dir, "\" is created in the current directory \"",
                getwd(), "\".", sep = ""))
  }

  wk_dir <- paste0(getwd(), "/", dir)
  if(!file.exists(wk_dir))
    # Check if a file name dir exist already
    dir.create(wk_dir)
  else
    message(cat("Simulations results will be saved in \"", wk_dir, "\".", sep = ""))

  # Loading experiment
  message(cat("Loading experiment \"", experiment,
              "\" from file \"", basename(model), "\"...", sep = ""))
  tmp <- tempfile(fileext = ".xml")
  system(paste0("java -jar ", getOption("rama.startjar"),
                " -Xms", getOption("rama.Xms"),
                " -Xmx", getOption("rama.Xmx"),
                " -Djava.awt.headless=true org.eclipse.core.launcher.Main",
                " -application msi.gama.headless.id4 -xml ",
                experiment, " ", model, " ", tmp, " > /dev/null"),
         ignore.stdout = TRUE, ignore.stderr = TRUE)
  unlink("workspace", TRUE, TRUE) # removes the above-created workspace directory
  out <- xmlToList(xmlParse(tmp))
  if (is.null(out)) {
    stop(
      paste0("There is no experiment named \"", experiment, "\" in \"",
             basename(model), "\"."))
  } else {
    out <- out$Simulation
  }
  out <- lapply(c(get_parameters, get_variables, get_attributes), function(f) f(out))
  names(out[[1]]) <- paste0("p_", names(out[[1]]))
  names(out[[2]]) <- paste0("r_", names(out[[2]]))
  out <- do.call(cbind, out)
  class(out) <- c("experiment", class(out))
  attr(out, "model") <- as.character(unname(out$gaml))
  attr(out, "experiment") <- as.character(unname(out$experiment))
  attr(out, "wkdir") <- wk_dir
  out$gaml <- NULL
  out$experiment <- NULL
  out
}




# save_to_gama -----------------------------------------------------------------

#' Save an experiment plan to a GAMA XML file
#'
#' Save an object of class \code{experiment} to an XML file GAMA-compliant.
#'
#' @param plan Object of class \code{experiment}.
#' @param file The path to the output XML file.
#'
#' @importFrom XML xmlToList xmlParse xmlOutputDOM saveXML
#'
#' @export
save_to_gama <- function(plan, file) UseMethod("save_to_gama")




#' @describeIn save_to_gama
#' @export
save_to_gama.experiment <- function(plan, file = "out.xml") {
  xmlFile <- xmlOutputDOM(tag = "Experiment_plan")
  id_simulation <- 0
  for(row_id in 1:nrow(plan)) {
    attrib <- c(id         = row_id,
                seed       = plan[row_id,]$seed,
                finalStep  = plan[row_id,]$tmax,
                sourcePath = model(plan),
                experiment = expname(plan))
    xmlFile$addTag("Simulation", attrs = attrib, close = FALSE)
    xmlFile$addTag("Parameters", close = FALSE)
    y <- parameters(plan[row_id, ])
    for(col_id in 1:ncol(y)) {
      param <- y[, col_id, drop = FALSE]
      title <- substr(names(param), 3, nchar(names(param)))
      val <- param[1, 1]
      m_type <- "STRING"
      if (is.numeric(val)) {
        if (is.integer(val)) {
          m_type <- "INT"
        } else m_type <- "FLOAT"
      }
      attribut <- c(var  = title,
                    type  = m_type,
                    value = val)
      xmlFile$addTag("Parameter", attrs = attribut)
    }
    xmlFile$closeTag()
    xmlFile$addTag("Outputs", close = FALSE)
    y <- observation(plan[row_id, ])
    id_out <- 0
    for(col_id in 1:ncol(y))
    {
      param <- y[, col_id, drop = FALSE]
      title <- substr(names(param), 3, nchar(names(param)))
      val <- param[1, 1]
      attribut <- c(id        = id_out,
                    name      = title,
                    framerate = val)
      id_out <- id_out + 1
      xmlFile$addTag("Output", attrs = attribut)
    }
    xmlFile$closeTag()
    xmlFile$closeTag()
  }
  xmlFile$closeTag()
  saveXML(xmlFile$value(), file)
  normalizePath(file)
}

#' @export
get_wkdir <- function(x) UseMethod("get_wkdir")

#' @export
get_wkdir.default <- function(x) "Unknown class"


# model ------------------------------------------------------------------------
#' @export
get_wkdir.experiment <- function(x) {
  attributes(x)$wkdir
}


model <- function(x) UseMethod("model")


model.default <- function(x) "Unknown class"


model.experiment <- function(x) {
  attributes(x)$model
}




# xepname ----------------------------------------------------------------------


expname <- function(x) UseMethod("expname")

expname.default <- function(x) "Unknown class"

expname.experiment <- function(x) {
  attributes(x)$experiment
}




# parameters -------------------------------------------------------------------

#' @export
parameters <- function(x) UseMethod("parameters")

#' @export
parameters.default <- function(x) "Unknown class"

#' @export
parameters.experiment <- function(x) {
  as.data.frame(x[, grep("^p_", names(x), value = TRUE), drop = FALSE])
}




# observation ------------------------------------------------------------------

#' @export
observation <- function(x) UseMethod("observation")

#' @export
observation.default <- function(x) "Unknown class"

#' @export
observation.experiment <- function(x) {
  as.data.frame(x[, grep("^r_", names(x), value = TRUE), drop = FALSE])
}




# repl -------------------------------------------------------------------------

#' @export
repl <- function(x, n) UseMethod("repl")

#' @export
repl.default <- function(x, n) "Unknown class"

#' @export
repl.experiment <- function(x, n) {
  do.call(rbind, lapply(1:n, function(y) x))
}




# experiment -------------------------------------------------------------------

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
#' # If we want to change the seeds:
#' my_experiment$seed <- 1:9
experiment <- function(parameters, obsrates, tmax, seed, model, experiment) {
  names(parameters) <- paste0("p_", names(parameters))
  names(obsrates) <- paste0("r_", names(obsrates))
  structure(data.frame(parameters, obsrates,
                       tmax = tmax,
                       seed = seed),
            model = model,
            experiment = experiment,
            class = c("experiment", "data.frame"))
}




# init_experiment --------------------------------------------------------------

#' @export
init_experiment <- function(df, model) {
  structure(df,
            model = model,
            class = c("experiment", "data.frame"))
}




# indexes_first_and_last -------------------------------------------------------

#' @param x A vector of characters.
#' @param n The number of elements to extract from the vector x. Should be > 1.
#'
#' @return A subvector of n elements of x
#'
#' @details If n = 2, it returns the first and the last elements, if n = 3, it
#'          returns the first 2 and the last elements, if n = 4, it returns the
#'          first 2 and the last 2 elements, if n = 5, it returns the first 3
#'          and the last 2 elements, and so on...
#'
#' @noRd
indexes_first_and_last <- function(x, n) {
  l <- length(x)
  x[c(1:rep(1:l, each = 2)[n], rep(l:1, each = 2)[n - 1]:l)]
}




# get_width --------------------------------------------------------------------

#' @param x A vector of characters.
#' @param n The targeted width, in number of characters, we would like.
#'
#' @return The actual width, in number of characters, we get.
#'
#' @noRd
get_width <- function(x, n) {
  x <- nchar(indexes_first_and_last(x, n))
  sum(x) + length(x) - 1
}




# names_of_left_and_right ------------------------------------------------------

#' @param x A vector of characters.
#' @param th The targeted width, in number of characters, we would like.
#'
#' @return A list of two vectors of characters. The first element corresponds to
#'         the left part and the second element corresponds to the right part.
#'
#' @noRd
names_of_left_and_right <- function(x, th) {
  tmp <- sapply(2:length(x), get_width, x = x) > th
  if (all(tmp)) tmp <- x[c(1, length(x))]
  else {
    if (all(! tmp)) tmp <- x
    else tmp <- indexes_first_and_last(x, which(tmp)[1])
  }
  sel <- 1:round(length(tmp) / 2)
  list(tmp[sel], tmp[-sel])
}




# insert_middle ----------------------------------------------------------------

#' @param x A data frame.
#' @param n The width, in number of characters we wish the data frame.
#'
#' @return A data frame with reduced number of columns.
#'
#' @examples
#' insert_middle(as.data.frame(exp5), 20)
#'
#' @noRd
insert_middle <- function(x, n, digits = 4) {
  x <- round(x, digits)
  a <- names_of_left_and_right(names(x), n)
  if (sum(sapply(a, length)) < length(x)) {
    left <- x[, a[[1]], drop = FALSE]
    right <- x[, a[[2]], drop = FALSE]
    middle <- setNames(data.frame(".", ".", ".", stringsAsFactors = FALSE), rep(".", 3))
    return(cbind(left, middle, right))
  }
  x
}




# print.experiment method ------------------------------------------------------

#' @export
print.experiment <- function(x, interspace = 3, n = 6, digits = 4, nchar = 50) {
  param <- parameters(x)
  obser <- observation(x)
  if (ncol(param) > 2) param2 <- insert_middle(param, nchar, digits)
  else param2 <- param
  if (ncol(obser) > 2) obser2 <- insert_middle(obser, nchar, digits)
  else obser2 <- obser
  y <- cbind(param2,
             obser2,
             x[, c("tmax", "seed")])
  if (nrow(y) > 2 * n + interspace) {
    h <- head(y, n)
    t <- tail(y, n)
    hn <- rownames(h)
    tn <- rownames(t)
    m <- setNames(as.data.frame(matrix(".", interspace, ncol(y)),
                                stringsAsFactors = FALSE), names(y))
    out <- rbind(h, m, t)
    out <- cbind(c(hn, rep(".", interspace), tn), out)
    names(out)[1] <- ""
    print(out, row.names = FALSE)
  } else print(y)
  cat("Linked to experiment \"", attributes(x)$experiment, "\" of model \"", attributes(x)$model, "\".\n", sep = "")
  cat("Outputs are saved in \"", attributes(x)$wkdir, "\".\n", sep = "")
  cat("Parameters are ", paste(names(param), collapse = ", "), ".\n", sep = "")
  cat("Observed variables are ", paste(names(obser), collapse = ", "), ".", sep = "")
  invisible(x)
}




# list_experiment --------------------------------------------------------------

#' List the experiments of a model
#'
#' List the experiments of a given model.
#'
#' @param x path to a gaml model file.
#'
#' @importFrom readtext readtext
#'
#' @export
#'
list_experiment <- function(x) {
  gaml <- readtext(x, verbosity = FALSE)
  gaml <- strsplit(gaml$text, "\n")[[1]]  # because strsplit returns a list
  gaml <- gaml[grepl("^ *experiment", gaml)]
  if (length(gaml) == 0) {
    stop("There is no experiment in this model.")
  }
  gaml <- sapply(gaml, function(x) strsplit(gsub("  *", " ", x), " "))
  sel <- unname(sapply(gaml, function(x) which(x == "experiment")) + 1)
  unname(unlist(Map(`[`, gaml, sel)))
}
