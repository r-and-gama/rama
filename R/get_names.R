# get names according to a pattern. Basically a wrapper around grep ------------
get_names <- function(pattern, text) {
  # unname(sapply(grep(pattern, text, value = TRUE),
  #              function(x) sub("^.*\\\"(.*)\\\".*$", "\\1", x)))
  str <- grep(pattern, text, value = TRUE)
  lst_str <- lapply(seq_along(str), function(x) {
    vect <- strsplit(str[x], " var|value")
    vect <- unlist(vect)[1]
    vect <- sub("^.*\\\"(.*)\\\".*$", "\\1", vect)
    vect
  })
  unlist(lst_str)
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
#' @example inst/examples/get_parameters_names.R
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


# get_info ----------------------------------------------------------------
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
    names <- paste0("p_", unlist(lapply(query, function(x) x[["name"]])) )
  if(pattern == "Outputs")
    names <- paste0("r_", unlist(lapply(query, function(x) x[["name"]])) )
  names(out) <- names
  out
}

# map gama and R data types
map_type <- function(x){
  types <- c("INT" = "integer", "FLOAT" = "numeric", "STRING" = "character")
  unlist(lapply(x, function(y) types[[y]]))
}
