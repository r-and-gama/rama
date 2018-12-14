# Make working directory -------------------------------------------------------
make_wkdir <- function(dir, model) {

  message(cat("Using current directory \"", getwd(), "\"...", sep = ""))

  if (dir == "") {
    # get model name from gaml file
    dir <- gsub(".gaml", "", basename(model))
    message(cat("Using default directory name \"", dir, "\"...", sep = ""))
  }

  i <- 0
  repeat {
    i <- i + 1
    wk_dir <- paste0(getwd(), "/", dir, "_", i)
    if (!file.exists(wk_dir)) break
  }

  dir.create(wk_dir)
  message(cat("Simulations results will be saved in \"", wk_dir,
              "\".", sep = ""))
  return(wk_dir)
}

# make_dictionary --------------------------------------------------------------

#' @importFrom stats setNames
make_dictionary <- function(x) {
  dic <- gsub("[[:space:]]|[[:punct:]]", "_", x)
  dic <- gsub("_+", "_", dic)
  dic <- setNames(dic, x)
}

# experiment -------------------------------------------------------------------
#' Create an object of class \code{experiment}
#'
#' @param df A data frame
#' @param parameters Vector of column names or indexes in the \code{df} that will be
#' used as parameters in the experiment.
#' @param obsrates Vector of column names or indexes in the \code{df} that will be
#' used as obs_rates rats in the experiment.
#' @param tmax Name or index of the column in the \code{df} that will be
#' used as final step in the experiment.
#' @param seed Name or index of the column in the \code{df} that will be
#' used as seed in the experiment.
#' @param experiment name to the model linked to the experiment
#' @param model path to the model linked to the experiment
#' @param dir Name of the output directory to be created in the current directory.
#' If not specified, name of the model will be used
#'
#' @importFrom dplyr case_when
#'
#' @examples
#' df <- data.frame("S0" = rep(999, 5), "I0" = rep(1, 5), "R0" = rep(0, 5),
#'                 "beta" = rep(1.5, 5), "gama" = runif (5, 0, 1),
#'                 "S" = rep(1, 5), "I" = rep(1, 5), "R" = rep(1, 5),
#'                 "a" = rep(1000, 5), "b" = rep(1, 5))
#' exp <- experiment(df,
#'                   parameters = c("S0", "I0", "R0", "beta", "gama"),
#'                   obsrates = c("S", "I", "R"),
#'                   tmax = "a",
#'                   seed = "b",
#'                   experiment = "sir",
#'                   model = system.file("examples", "sir.gaml", package = "rama"))
#' exp <- experiment(df,
#'                   parameters = c(1:5),
#'                   obsrates = c(6:8),
#'                   tmax = 9,
#'                   seed = 10,
#'                   experiment = "sir",
#'                   model = system.file("examples", "sir.gaml", package = "rama"),
#'                   dir = "my_sir_model")
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
                               dir = "") "Unknown class"


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
                                  dir = "") {
  if (is.null(parameters) || is.null(obsrates) ||
     is.null(tmax) || is.null(seed) ||
     is.null(experiment) || is.null(model))
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
                  "class" = c("experiment", "data.frame"))
  names(df) <- c(parameters_n, obsrates_n, "tmax", "seed")
  return(df)
}
