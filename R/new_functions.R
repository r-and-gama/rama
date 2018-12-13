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
  out <- setNames(do.call(function(...) data.frame(..., stringsAsFactors = FALSE),
                          as.list(x$.attrs[c("finalStep", "seed", "sourcePath", "experiment")])),
                  c("tmax", "seed", "gaml", "experiment"))
  out$tmax <- as.integer(out$tmax)
  out$seed <- as.numeric(out$seed)
  out
}




# make_dictionary --------------------------------------------------------------

#' @importFrom stats setNames
make_dictionary <- function(x) {
  dic <- gsub("[[:space:]]|[[:punct:]]", "_", x)
  dic <- gsub("_+", "_", dic)
  dic <- setNames(dic, x)
}


# test special character -------------------------------------------------------
test_schar <- function(x) {
  if (any(grepl("[\\&|\\<|\\>|\\']", x))) {
    stop(paste0("Rama package does not support these specials characters : `<`",
                ", `>`, `&` and `'` in parameters, outputs and experiment name",
                ". Please rewrite them without these specials characters"))
  }
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
#' @param experiment The name of the experiment to load.
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
load_experiment <- function(experiment, model, dir = "") {

  # Check if experiment and type requested are valid
  check_experiment(experiment, model)
  # Make working directory

  wk_dir <- make_wkdir(dir, model)

  # Loading experiment
  message(cat("Loading experiment \"", experiment,
              "\" from file \"", basename(model), "\"...", sep = ""))
  tmp <- tempfile(fileext = ".xml")
  system(paste0("java -jar ", getOption("rama.startjar"),
                " -Xms", getOption("rama.Xms"),
                " -Xmx", getOption("rama.Xmx"),
                " -Djava.awt.headless=true org.eclipse.core.launcher.Main",
                " -application msi.gama.headless.id4 -xml ",
                experiment, " '", model, "' ", tmp, " > /dev/null"),
         ignore.stdout = TRUE, ignore.stderr = TRUE)
  unlink("workspace", TRUE, TRUE) # removes the above-created workspace directory

  if(file.exists(tmp)) {
    out <- XML::xmlToList(XML::xmlParse(tmp))
  } else {
    stop(paste0("Gama fails to read your experiment"))
  }

  out <- out$Simulation
  if (!is.null(out$Outputs)) {
    out_var <- get_variables(out)
    dicar <- make_dictionary(out_var)
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
  return(output)
}




# save_to_gama -----------------------------------------------------------------

#' Save an experiment plan to a GAMA XML file
#'
#' Save an object of class \code{experiment} to an XML file GAMA-compliant.
#'
#' @param plan An object of class \code{experiment}.
#' @param file The path to the output XML file.
#'
#' @importFrom XML xmlToList xmlParse xmlOutputDOM saveXML
#'
#' @export
save_to_gama <- function(plan, file) UseMethod("save_to_gama")




#' @rdname save_to_gama
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
      title <- as.vector(attr(plan, "dic_rev")[title])
      val <- param[1, 1]
      m_type <- "STRING"
      if (is.numeric(val)) {
        if (is.integer(val)) {
          m_type <- "INT"
        } else m_type <- "FLOAT"
      }
      attribut <- c(name = title,
                    type = m_type,
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
      title <- as.vector(attr(plan, "dic_rev")[title])
      val <- param[1, 1]
      attribut <- c(id        = id_out,
                    name = title,
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




# get_wkdir --------------------------------------------------------------------

#' Get and set the output directory
#'
#' These functions allow to get and set the path to the directory where the
#' simulations outputs will be saved.
#'
#' @param x An object of class \code{experiment}.
#'
#' @return The path of the directory where the simulations outputs will be saved.
#'
#' @examples
#' exp1 <- load_experiment("sir", system.file("examples", "sir.gaml", package = "rama"))
#' get_wkdir(exp1)
#'
#' @export
#'
get_wkdir <- function(x) UseMethod("get_wkdir")




#' @rdname get_wkdir
#' @export
get_wkdir.default <- function(x) "Unknown class"




#' @rdname get_wkdir
#' @export
get_wkdir.experiment <- function(x) attributes(x)$wkdir




# model ------------------------------------------------------------------------

#' Get and set the model of an experiment
#'
#' These functions allow to get and set the path to the \code{.gaml} file that
#' contains the model of an \code{experiment} object.
#'
#' @param x An object of class \code{experiment}.
#'
#' @return The path to the \code{.gaml} file that contains the model definition.
#'
#' @examples
#' exp1 <- load_experiment("sir", system.file("examples", "sir.gaml", package = "rama"))
#' model(exp1)
#'
#' @export
#'
model <- function(x) UseMethod("model")




#' @rdname model
#' @export
model.default <- function(x) "Unknown class"




#' @rdname model
#' @export
model.experiment <- function(x) attributes(x)$model




# expname ----------------------------------------------------------------------

#' Get the name of an experiment
#'
#' Retrieves the name of the experiment that an \code{experiment} object is
#' linked to.
#'
#' @param x An object of class \code{experiment}.
#'
#' @return The name of the experiment that the inputed \code{experiment} object
#' is linked to.
#'
#' @examples
#' exp1 <- load_experiment("sir", system.file("examples", "sir.gaml", package = "rama"))
#' expname(exp1)
#'
#' @export
#'
expname <- function(x) UseMethod("expname")




#' @rdname expname
#' @export
expname.default <- function(x) "Unknown class"




#' @rdname expname
#' @export
expname.experiment <- function(x) attributes(x)$experiment




# parameters -------------------------------------------------------------------

#' Extract parameters values
#'
#' Subsets the columns of an \code{experiment} object that correspond to the
#' values of the parameters.
#'
#' @param x An object of class \code{experiment}.
#'
#' @return A data frame that is a subset of the inputed \code{experiment} object.
#'
#' @examples
#' exp1 <- load_experiment("sir", system.file("examples", "sir.gaml", package = "rama"), "sir")
#' exp2 <- repl(exp1, 10)
#' parameters(exp2)
#'
#' @export
parameters <- function(x) UseMethod("parameters")




#' @rdname parameters
#' @export
parameters.default <- function(x) "Unknown class"




#' @rdname parameters
#' @export
parameters.experiment <- function(x) {
  as.data.frame(x[, grep("^p_", names(x), value = TRUE), drop = FALSE])
}




# observation ------------------------------------------------------------------

#' Extract monitoring rates
#'
#' Subsets the columns of an \code{experiment} object that correspond to the
#' observation rates of the monitored variables.
#'
#' @param x An object of class \code{experiment}.
#'
#' @return A data frame that is a subset of the inputed \code{experiment} object.
#'
#' @examples
#' exp1 <- load_experiment("sir", system.file("examples", "sir.gaml", package = "rama"), "sir")
#' exp2 <- repl(exp1, 10)
#' observation(exp2)
#'
#' @export
observation <- function(x) UseMethod("observation")




#' @rdname observation
#' @export
observation.default <- function(x) "Unknown class"




#' @rdname observation
#' @export
observation.experiment <- function(x) {
  as.data.frame(x[, grep("^r_", names(x), value = TRUE), drop = FALSE])
}




# repl -------------------------------------------------------------------------

#' Replicate an experiment
#'
#' Produces an \code{experiment} object from the replication of an
#' \code{experiment} object.
#'
#' @param x An object of class \code{experiment}.
#' @param n The number of times to repeat the experiment \code{x}.
#'
#' @return An object of class \code{experiment} that is the replication of the
#' inputed \code{x} argument.
#'
#' @export
repl <- function(x, n) UseMethod("repl")




#' @rdname repl
#' @export
repl.default <- function(x, n) "Unknown class"




#' @rdname repl
#' @export
repl.experiment <- function(x, n) {
  do.call(rbind, lapply(1:n, function(y) x))
}




# experiment -------------------------------------------------------------------
#' Create an object of class \code{experiment} from a dataframe.
#'
#' @param df A data frame
#' @param parameters Vector of column names or indexes in the \code{df} that will be
#' used as parameters in the experiment.
#' @param obsrates Vector of column names or indexes in the \code{df} that will be
#' used as observation rats in the experiment.
#' @param tmax Name or index of the column in the \code{df} that will be
#' used as final step in the experiment.
#' @param seed Name or index of the column in the \code{df} that will be
#' used as seed in the experiment.
#' @param dir Name of the output directory to be created in the current directory.
#' If not specified, name of the model will be used
#'
#' @importFrom dplyr case_when
#'
#' @examples
#' df <- data.frame("S0" = rep(999, 5), "I0" = rep(1, 5), "R0" = rep(0, 5),
#'                 "beta" = rep(1.5, 5), "gama" = runif(5, 0, 1),
#'                 "S" = rep(1, 5), "I" = rep(1, 5), "R" = rep(1, 5),
#'                 "a" = rep(1000, 5), "b" = rep(1, 5))
#' exp <- experiment(df,
#                    parameters = c("S0", "I0", "R0", "beta", "gama"),
#'                   obsrates = c("S", "I", "R"),
#'                   tmax = "a",
#'                   seed = "b")
#' exp <- experiment(df,
#'                   parameter = c(1:5),
#'                   obsrates(6:8)),
#'                   tmax = 9,
#'                   seed = 10)
#'
#'
#'
#' @export
experiment <- function(df,
                       parameters = NULL,
                       obsrates = NULL,
                       tmax = NULL,
                       seed = NULL,
                       experiment = NULL,
                       model = NULL,
                       dir = "") UseMethod("experiment")


#' @rdname experiment
#' @export
experiment.default <- function(df,
                               parameters = NULL,
                               obsrates = NULL,
                               tmax = NULL,
                               seed = NULL,
                               experiment = NULL,
                               model = NULL,
                               dir = ""
) "Unknown class"


# experiment constructor from a dataframe --------------------
#' @rdname experiment
#' @export
experiment.data.frame <- function(df,
                                  parameters = NULL,
                                  obsrates = NULL,
                                  tmax = NULL,
                                  seed = NULL,
                                  experiment = NULL,
                                  model = NULL,
                                  dir = ""
                                  )
{
  if(is.null(parameters) || is.null(obsrates) ||
     is.null(tmax) || is.null(seed) ||
     is.null(experiment) || is.null(model))
    stop(paste0("All parameters need to be set."))

  if(length(tmax) > 1 || length(seed) > 1)
    stop(paste0("tmax and seed take only one column"))

  if(!file.exists(model))
    stop(paste0("Model \"", model, "\" does not exist"))

  if(sum(length(parameters) + length(obsrates) +
         length(tmax) + length(seed)) > ncol(df))
    stop(paste0("Column(s) selected is out of bound"))

  # check if requested name is in df
  if (is.character(parameters) && sum(parameters %in% names(df)) == 0)
    stop(paste0("Requested column(s) for parameters not found."))
  if (is.character(obsrates) && sum(obsrates %in% names(df)) == 0)
    stop(paste0("Requested column(s) for obsrates not found."))
  if(is.character(tmax) && !tmax %in% names(df))
    stop(paste0("Requested column for tmax not found."))
  if(is.character(seed) && !seed %in% names(df))
    stop(paste0("Requested column(s) for seed not found."))
  # check experiment and type
  check_experiment(experiment, model)
  # generate output dir
  wk_dir <- make_wkdir(dir, model)
  parameters_n <- dplyr::case_when(
    is.character(parameters) ~ paste0("p_", parameters),
    is.numeric(parameters) ~ paste0("p_", names(df)[parameters])
  )
  obsrates_n <- dplyr::case_when(
    is.character(obsrates) ~ paste0("r_", obsrates),
    is.numeric(obsrates) ~ paste0("r_", names(df)[obsrates])
  )

 tmax_n <- dplyr::case_when(
   is.character(tmax) ~ tmax,
   is.numeric(tmax) ~ names(df)[tmax]
 )

 seed_n <- dplyr::case_when(
   is.character(tmax) ~ seed,
   is.numeric(tmax) ~ names(df)[seed]
 )
  dic_n <- make_dictionary(c(parameters_n, obsrates_n))
  df <- structure(data.frame(df[parameters], df[obsrates], df[tmax], df[seed]),
            "model" = model,
            "experiment" = experiment,
            "wkdir" = wk_dir,
            "dic" = dic_n,
            "dic_rev" = setNames(names(dic_n), dic_n),
            "class" = c("experiment", "data.frame"))
  names(df) <- c(parameters_n, obsrates_n, "tmax", "seed")
  return(df)
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
print.experiment <- function(x, interspace = 3, n = 6, digits = 4, nchar = 50, ...) {

  attrs <- attributes(x)

  print_info <- function() {
    cat(  "experiment name:    ", attrs$experiment,
        "\ninput gaml file:    ", attrs$model,
        "\noutput directory:   ", attrs$wkdir, "\n")
  }

  if (ncol(x) < 1) {

    cat("Experiment without any simulation, tunable parameter or observed variable\n")
    print_info()

  } else {

    s <- function(x) ifelse(x > 1, "s", "")
    param <- parameters(x)
    obser <- observation(x)
    nsim <- nrow(x)
    npar <- ncol(param)
    nvar <- ncol(obser)

    cat("Experiment with ", nsim, " simulation"       , s(nsim),
        " of "            , npar, " parameter"        , s(npar),
        " and "           , nvar, " observed variable", s(nvar), "\n", sep = "")
    print_info()
    cat("model parameters:   "  , paste(names(param), collapse = ", "),
        "\nobserved variables: ", paste(names(obser), collapse = ", "),
        "\nExperiment overview:\n")

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

  }
  invisible(x)
}



# show_experiment --------------------------------------------------------------

#' List the experiments of a model and their types
#'
#' List the experiments of a given model.
#'
#' @param file path to a gaml model file.
#'
#' @importFrom stringr str_match_all str_match regex str_detect
#' @importFrom  purrr map
#'
#' @export
#'
#'
show_experiment <- function(file){
  if (!file.exists(file)) {
    stop(paste0("There is no file \"", file, "\"."))
  }

  gaml <- paste(readLines(file, warn = FALSE), collapse = "\n")
  exps <- str_match_all(gaml, regex("\\nexperiment (.*?)\\{", dotall = T))[[1]][,2]

  if(length(exps) == 0)
    stop(paste0("Model \"", file, "\" does not contain any experiment."))
  exps <- trimws(gsub("\\n+$","",exps))
  exp_info <- purrr::map(exps, function(x){
    if(str_detect(x, "type"))
      tmp <- cbind(str_match(x, ".*?(?=\\s+type?)"),
                   trimws(str_match(x, "type\\:(.*)"))[,2])
    else
      tmp <- cbind(x, "gui")
  })
  exp_info <- as.data.frame(do.call(rbind, lapply(exp_info, function(x) x)))
  names(exp_info) <- c("experiment", "type")
  exp_info$experiment <- as.character(exp_info$experiment)
  # test if there is special character in experiment name
  test_schar(exp_info$experiment)
  exp_info$type <- as.character(exp_info$type)
  return(exp_info)
}

#' Test if experiment
#'
#' Tests for objects of type \code{"experiment"}.
#'
#' @param x object to be tested
#'
#' @return The function returns `TRUE` or `FALSE` depending on whether its
#' argument is of chatacter type or not
#'
#' @examples
#' gaml_file <- system.file("examples", "sir.gaml", package = "rama")
#' exp1 <- load_experiment("sir", gaml_file, "sir")
#'
#' is.experiment(exp1)
#' @export
is.experiment <- function(x) {

  if(any(is.na(x))) stop("An object `experiment` can not contain NA value.")
  attr <- setdiif(c("class", "model", "experiment", "wkdir", "dic", "dic_rev"),
                  names(attributes(x)))
  class <- setdiff(class(x), c("data.frame", "experiment"))
  length(c(attr, class)) == 0
}
# Check if a requested experiment is valid: name exists and type = gui----------

check_experiment <- function(experiment, model){
  exp_info <- show_experiment(model)
  # check if experiment requested is declared in gaml
  if(!experiment %in% exp_info$experiment)
    stop(paste0("There is no experiment named \"", experiment, "\" in ",
                basename(model)))
  # check if experiment requested has valid type
  type <- exp_info$type[exp_info$experiment == experiment]
  if(type != "gui")
    stop(paste0("Experiment \"", experiment, "\" of type \"", type, "\" is not supported."))
  invisible(0)
}

# Make working directory ----------
make_wkdir <- function(dir, model) {

  message(cat("Using current directory \"", getwd(), "\"...", sep = ""))

  if(dir == "") {
    # get model name from gaml file
    dir <- gsub(".gaml", "", basename(model))
    message(cat("Using default directory name \"", dir, "\"...", sep = ""))
  }

  wk_dir <- paste0(getwd(), "/", dir)

  if (file.exists(wk_dir)) {
    stop(paste0("Directory \"", dir, "\" already exists in \"", getwd(), "\""))
  } else {
    # Check if a file name dir exist already
    dir.create(wk_dir)
    message(cat("Simulations results will be saved in \"", wk_dir,
                "\".", sep = ""))
  }
  return(wk_dir)
}



# $<-.experiment ---------------------------------------------------------------

#' Replace a column of an experiment
#'
#' Replaces a column of an experiment with new value(s).
#'
#' If the length of the vector used to replace the column is not the same as the
#' original number of rows of the experiment, there is duplication of the
#' shortest element.
#'
#' @param x An object of class \code{experiment}.
#' @param i A column index.
#' @param value A vector used to replace the values of the indexed column.
#'
#' @return An object of class \code{experiment}.
#'
#' @examples
#' # Here is an experiment with 1 simulation:
#' sir1 <- load_experiment("sir", system.file("examples", "sir.gaml", package = "rama"), "sir")
#' sir1
#' # Let's replace the value of the "p_S0" column by a vector of 3 values:
#' sir2 <- sir1
#' sir2$p_S0 <- 1:3
#' # We can check that it automatically expands the number of simulations:
#' sir2
#' # If, on the contrary, we now replace the values of "p_S0" of "sir2" by a
#' # single value:
#' sir3 <- sir2
#' sir3$p_S0 <- 2
#' # We can check that it automatically reduces the number of simulations (if
#' # the replacement leads to an experiment with exactly identical simulations):
#' sir3
#' # If you wish to delete one column:
#' sir3$r_R <- NULL
#' sir3
#'
#' @export
`$<-.experiment` <- function(x, i, value) {
  if (is.null(value)) NextMethod()
  else {
    x_list <- as.list(x)
    x_list[[i]] <- value
    new_x <- do.call(function(...) data.frame(..., stringsAsFactors = FALSE), x_list)
    unique(rbind(x[1, ], new_x)[-1, ])
  }
}

