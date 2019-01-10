# Make working directory -------------------------------------------------------
# Use full path dir if specified. If only name specified, use current directory.
# If not specified, use default name.
make_wkdir <- function(dir, model) {

  if (dir == "") {
    # get model name from gaml file
    dir <- gsub(".gaml", "", basename(model))
    message(cat("Using default directory name \"", dir,
                "\" in current directory \"", getwd(), "\".", sep = ""))
  }
     if (dir.exists(dir)) {
    i <- 0
    repeat {
      i <- i + 1
      wk_dir <- paste0(dir, "_", i)
      if (!file.exists(wk_dir)) break
    }
  } else {
    wk_dir <-  dir
  }

  dir.create(wk_dir, recursive = TRUE)
  message(cat("Simulations results will be saved in \"", wk_dir,
              "\".", sep = ""))

  wk_dir
}

# make_dictionary --------------------------------------------------------------

#' @importFrom stats setNames
make_dictionary <- function(x) {
  dic <- gsub("[[:space:]]|[[:punct:]]", "_", x)
  dic <- gsub("_+", "_", dic)
  setNames(dic, x)
}

# experiment constructor -------------------------------------------------------
#' Create an object of class \code{experiment}
#'
#' @param parameters Vector of column names or indexes in the \code{df} that will be
#' used as parameters in the experiment.
#' @param obsrates Vector of column names or indexes in the \code{df} that will be
#' used as obs_rates rats in the experiment.
#' @param tmax Name or index of the column in the \code{df} that will be
#'             used as final step in the experiment.
#' @param seed Name or index of the column in the \code{df} that will be
#'             used as seed in the experiment.
#' @param experiment name to the model linked to the experiment
#' @param model path to the model linked to the experiment
#' @param dir Either absolute path or name output of directory. In the latter
#'            case, current directory will be used. If \code{dir} is not
#'            specified, name of model will be used to create an output
#'            directory in the current directory.
#' @param df A data frame used to initialize an experiment object.
#' @param ... Additional paramaters
#'
#' @importFrom dplyr case_when
#' @examples
#' # Experiment constructor with a dataframe and indexes/names of columns
#' # indicating parameters, observation rates, tmax and seed
#'
#' df <- data.frame("S0" = rep(999, 5), "I0" = rep(1, 5), "R0" = rep(0, 5),
#'                 "beta" = rep(1.5, 5), "gama" = runif (5, 0, 1),
#'                 "S" = rep(1, 5), "I" = rep(1, 5), "R" = rep(1, 5),
#'                 "a" = rep(1000, 5), "b" = rep(1, 5))
#' exp1 <- experiment(parameters = c("S0", "I0", "R0", "beta", "gama"),
#'                   obsrates = c("S", "I", "R"),
#'                   tmax = "a",
#'                   seed = "b",
#'                   experiment = "sir",
#'                   model = system.file("examples", "sir.gaml", package = "rama"),
#'                   df = df)
#' exp2 <- experiment(parameters = c(1:5),
#'                   obsrates = c(6:8),
#'                   tmax = 9,
#'                   seed = 10,
#'                   experiment = "sir",
#'                   model = system.file("examples", "sir.gaml", package = "rama"),
#'                   dir = "my_sir_model",
#'                   df = df)
#'
#' # Experiment constructor that uses for data frames (of paramaters, observation
#' # rates, tmax and seed) as input.
#'
#' df1 <- data.frame("S0" = rep(999, 5), "I0" = rep(1, 5), "R0" = rep(0, 5),
#'                  "beta" = rep(1.5, 5), "gama" = runif (5, 0, 1))
#' df2 <- data.frame("S" = rep(1, 5), "I" = rep(1, 5), "R" = rep(1, 5))
#' tmax <- rep(1000, 5)
#' seed <- rep(1, 5)
#' exp3 <- experiment(parameters = df1,
#'                    obsrates = df2,
#'                    tmax = tmax,
#'                    seed = seed,
#'                    experiment = "sir",
#'                    model = system.file("examples", "sir.gaml", package = "rama"))
#'

#' @export

experiment <- function(parameters, obsrates, tmax, seed,
                       experiment, model, dir, df)
                      UseMethod("experiment")

#' @rdname experiment
#' @export
experiment.default <- function(parameters, obsrates, tmax, seed,
                               experiment, model, dir, df)
                      "Unknown class"


# experiment constructor from a dataframe and names of cols--------------------
#' @rdname experiment
#' @export
experiment.character <- function(parameters = NULL,
                                 obsrates = NULL,
                                 tmax = NULL,
                                 seed = NULL,
                                 experiment = NULL,
                                 model = NULL,
                                 dir = "",
                                 df = NULL) {
  if (is.null(parameters) || is.null(obsrates) ||
     is.null(tmax) || is.null(seed) ||
     is.null(experiment) || is.null(model) || is.null(df))
    stop(paste0("All parameters need to be set."))

  if (length(tmax) > 1 || length(seed) > 1)
    stop(paste0("tmax and seed take only one column"))

  if (!file.exists(model))
    stop(paste0("Model \"", model, "\" does not exist"))

  if (sum(length(parameters) + length(obsrates) +
         length(tmax) + length(seed)) > ncol(df))
    stop(paste0("Column(s) selected is out of bound"))

  # check if requested name is in df
  if (is.character(parameters) && sum(parameters %in% names(df)) == 0)
    stop(paste0("Requested column(s) for parameters not found."))
  if (is.character(obsrates) && sum(obsrates %in% names(df)) == 0)
    stop(paste0("Requested column(s) for obsrates not found."))
  if (is.character(tmax) && !tmax %in% names(df))
    stop(paste0("Requested column for tmax not found."))
  if (is.character(seed) && !seed %in% names(df))
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

  if (is.character(tmax)) tmax_n <- tmax
  if (is.numeric(tmax)) tmax_n <- names(df)[tmax]
  if (is.character(seed)) seed_n <- seed
  if (is.numeric(seed)) seed_n <- names(df)[seed]
  if (is.numeric(parameters) & is.numeric(obsrates))
    dic_n <- make_dictionary(c(names(df)[parameters], names(df)[obsrates]))
  else
    dic_n <- make_dictionary(c(parameters, obsrates))

  df <- structure(data.frame(df[parameters], df[obsrates], df[tmax], df[seed]),
                  "model" = model,
                  "experiment" = experiment,
                  "wkdir" = wk_dir,
                  "dic" = dic_n,
                  "dic_rev" = setNames(names(dic_n), dic_n),
                  "class" = c("experiment", "tbl_df", "tbl", "data.frame"))
  names(df) <- c(parameters_n, obsrates_n, "tmax", "seed")
  return(df)
}

# experiment constructor from a dataframe and indexes of cols-----------------
#' @rdname experiment
#' @export
experiment.numeric <- function(parameters = NULL,
                               obsrates = NULL,
                               tmax = NULL,
                               seed = NULL,
                               experiment = NULL,
                               model = NULL,
                               dir = "",
                               df = NULL) {
  if (is.null(parameters) || is.null(obsrates) ||
      is.null(tmax) || is.null(seed) ||
      is.null(experiment) || is.null(model) || is.null(df))
    stop(paste0("All parameters need to be set."))

  if (length(tmax) > 1 || length(seed) > 1)
    stop(paste0("tmax and seed take only one column"))

  if (!file.exists(model))
    stop(paste0("Model \"", model, "\" does not exist"))

  if (sum(length(parameters) + length(obsrates) +
          length(tmax) + length(seed)) > ncol(df))
    stop(paste0("Column(s) selected is out of bound"))

  # check if requested name is in df
  if (is.character(parameters) && sum(parameters %in% names(df)) == 0)
    stop(paste0("Requested column(s) for parameters not found."))
  if (is.character(obsrates) && sum(obsrates %in% names(df)) == 0)
    stop(paste0("Requested column(s) for obsrates not found."))
  if (is.character(tmax) && !tmax %in% names(df))
    stop(paste0("Requested column for tmax not found."))
  if (is.character(seed) && !seed %in% names(df))
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

  if (is.character(tmax)) tmax_n <- tmax
  if (is.numeric(tmax)) tmax_n <- names(df)[tmax]
  if (is.character(seed)) seed_n <- seed
  if (is.numeric(seed)) seed_n <- names(df)[seed]
  if (is.numeric(parameters) & is.numeric(obsrates))
    dic_n <- make_dictionary(c(names(df)[parameters], names(df)[obsrates]))
  else
    dic_n <- make_dictionary(c(parameters, obsrates))

  df <- structure(data.frame(df[parameters], df[obsrates], df[tmax], df[seed]),
                  "model" = model,
                  "experiment" = experiment,
                  "wkdir" = wk_dir,
                  "dic" = dic_n,
                  "dic_rev" = setNames(names(dic_n), dic_n),
                  "class" = c("experiment", "tbl_df", "tbl", "data.frame"))
  names(df) <- c(parameters_n, obsrates_n, "tmax", "seed")
  return(df)
}

# experment constructor from a group of data frame -----------------------------
#' @rdname experiment
#' @export
experiment.data.frame <- function(parameters = NULL,
                            obsrates = NULL,
                            tmax = NULL,
                            seed = NULL,
                            experiment = NULL,
                            model = NULL,
                            dir = "") {
  df <- cbind(parameters, obsrates, tmax, seed)
  exp <- experiment(names(parameters),
                    names(obsrates),
                    "tmax",
                    "seed",
                    experiment,
                    model,
                    dir,
                    df)
  return(exp)
}
# experiment constructor from a data frame and an experiment as template--------
#' Create an experiment object using a data frame and an experiment object
#' as template
#'
#' @param df A data frame used to initialize an experiment object.
#' @param exp An experiment object used as template.
#' @param dir Either absolute path or name output of directory. In the latter
#'            case, current directory will be used. If \code{dir} is not
#'            specified, name of model will be used to create an output
#'            directory in the current directory.
#'
#' @examples
#' exp1 <- load_experiment("sir", system.file("examples", "sir.gaml", package = "rama"))
#' df <- data.frame(matrix(1, nrow=5, ncol=12))
#' exp2 <- map_experiment(df, exp1)
#'
#' @importFrom stringr str_match
#' @importFrom stats na.omit
#' @importFrom utils capture.output
#' @export
map_experiment <- function(df, exp, dir = ""){
  # check ncol(df) >= para + obsrates + tmax + seed
  if (ncol(df) < ncol(exp))
    stop(paste0("Number of columns in data frame is not valid
                for the requested experiment."))
  names(df) <- c(attr(exp, "dic"), "tmax", "seed",
                 names(df)[(ncol(exp) + 1) : ncol(df)])
  params <- na.omit(stringr::str_match(names(exp), "p_(.*)")[, 2])
  obs <- na.omit(stringr::str_match(names(exp), "r_(.*)")[, 2])
  experiment(parameters = params,
             obsrates = obs,
             tmax = "tmax",
             seed = "seed",
             experiment = name(exp),
             model = model(exp),
             dir = dir,
             df = df)
}
