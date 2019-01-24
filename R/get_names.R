# get names according to a pattern. Basically a wrapper around grep ------------
get_names <- function(pattern, text) {
  unname(sapply(grep(pattern, text, value = TRUE),
                function(x) sub("^.*\\\"(.*)\\\".*$", "\\1", x)))
}



# A template to get_parameters_names() and get_variables_names() ---------------
get_names_template <- function(x) {
  function(file) get_names(x, readLines(file))
}



# get_parameters_names ---------------------------------------------------------
#' Get variables and parameters names
#'
#' Retrieves the names of the parameters and variables of model in a GAML file.
#'
#' The GAML file is parsed directly from R and does not requires to launch java
#' and GAMA. Thus, much faster!
#'
#' @param file Name of a GAML file.
#'
#' @return A vector of parameters and / or variables names.
#'
#' @export
#'
#' @examples
#' file <- system.file("examples", "sir.gaml", package = "rama")
#' get_parameters_names(file)
#' get_variables_names(file)
#' get_all_names(file)
#'
get_parameters_names <- get_names_template("parameter")



# get_variables_names ----------------------------------------------------------
#' @rdname get_parameters_names
#' @export
get_variables_names <- get_names_template("monitor")



# get_all_names ----------------------------------------------------------------
#' @rdname get_parameters_names
#' @export
get_all_names <- function(file) {
  unlist(lapply(c("parameter", "monitor"), get_names, readLines(file)))
}
