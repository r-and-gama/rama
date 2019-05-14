# get_info ---------------------------------------------------------------------
#' Get information on the model
#'
#' Get information on the model defined in the GAML file linked to an
#' \code{experiment} object.
#'
#' @param exp object of class \code{experiment}.
#' @param pattern can be either \code{Parameters} or \code{Outputs} for
#'                parameters and observation rates, respectively.
#' @param type can be either \code{name, type, value, var} and \code{name,
#'             framerate, id} for parameters and observation rates respectively.
#'
#' @return A vector of parameters or variables names, or a vector of parameters
#' types.
#'
#' @example inst/examples/get_info.R
#' @noRd
#' @importFrom dplyr case_when

get_info <- function(exp, pattern, type) {
  model <- model(exp)
  query <- model[["info"]][[pattern]]
  if (!is.list(query)) query <- list(query)
  out <- unlist(lapply(query, function(x) x[[type]]))

  # names <- case_when(
  #   pattern == "Parameters" ~ attr(exp, "dic_g2r")[unlist(
  #                                     lapply(query, function(x) x[["name"]]))],
  #   pattern == "Outputs" ~ attr(exp, "dic_g2r")[unlist(
  #                                     lapply(query, function(x) x[["name"]]))],
  #   pattern == ".attrs" ~ type
  #   )

  if(pattern == "Parameters")
    names <- attr(exp, "dic_g2r")[unlist(lapply(query, function(x) x[["name"]]))]
  if(pattern == "Outputs")
    names <- attr(exp, "dic_g2r")[unlist(lapply(query, function(x) x[["name"]]))]
  if(pattern == ".attrs")
    names <- type

 names(out) <- names
  out
}

# map gama and R data types
map_type <- function(x) {
  types <- c("INT" = "integer", "FLOAT" = "numeric", "STRING" = "character")
  unlist(lapply(x, function(y) types[[y]]))
}
