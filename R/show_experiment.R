#' List the experiments of a model and their types
#'
#' List the experiments of a given model.
#'
#' @param file Path to a \code{.gaml} file.
#'
#' @importFrom stringr str_match_all str_match regex str_detect
#' @importFrom purrr map
#'
#' @example inst/examples/show_experiment.R
#' @export
show_experiment <- function(file){
  if (!file.exists(file)) {
    stop(paste0("File \"", file, "\" does not exist."))
  }

  gaml <- paste(readLines(file, warn = FALSE), collapse = "\n")
  exps <- str_match_all(gaml,
                        regex("\\nexperiment (.*?)\\{", dotall = TRUE))[[1:2]]

  if (length(exps) < 1)
    stop(paste0("File \"", file, "\" does not contain any experiment."))
  exps <- trimws(gsub("\\n+$", "", exps))
  exp_info <- purrr::map(exps, function(x) {
    if (str_detect(x, "type"))
      tmp <- cbind(str_match(x, ".*?(?=\\s+type?)"),
                   trimws(str_match(x, "type\\:(.*)"))[, 2])
    else
      tmp <- cbind(x, "gui") # because, in GAMA, default type is GUI
  })

  exp_info <- as.data.frame(do.call(rbind, exp_info), stringsAsFactors = FALSE)
  names(exp_info) <- c("experiment", "type")
  # test if there is special character in experiment name
  test_schar(exp_info$experiment)
  exp_info
}
