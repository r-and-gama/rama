# get_info ---------------------------------------------------------
#' Get information on the model
#'
#' @param exp Experiment object
#' @param pattern can be either 'Parameters' or 'Outputs' for parameters and
#'        observation rates, respectively.
#' @param type can be 'name' for parameter and observation rate names, '
#'        type' for parameter types.
#'
#' @return A vector of parameter or variable names,
#'
#' @export
#'
#' @example inst/examples/get_info.R
#' @rdname get_info
#' @export
get_info <- function(exp, pattern, type){
  model <- model(exp)
  query <- model[["info"]][[pattern]]
  if(!is.list(query))
    query <- list(query)
  out <- unlist(lapply(query, function(x) x[[type]]))
  names <- type

  if(pattern == "Parameters")
    names <- attr(exp, "dic_g2r")[unlist(lapply(query, function(x) x[["name"]]))]
  if(pattern == "Outputs")
    names <- attr(exp, "dic_g2r")[unlist(lapply(query, function(x) x[["name"]]))]
  names(out) <- names
  out
}

# map gama and R data types
map_type <- function(x){
  types <- c("INT" = "integer", "FLOAT" = "numeric", "STRING" = "character")
  unlist(lapply(x, function(y) types[[y]]))
}
