# test special character -------------------------------------------------------
test_schar <- function(x) {
  if (any(grepl("[\\&|\\<|\\>|\\']", x))) {
    stop(paste0("Rama package does not support these specials characters : `<`",
                ", `>`, `&` and `'` in parameters, outputs and experiment name",
                ". Please rewrite them without these specials characters"))
  }
}

#show_experiment --------------------------------------------------------------
#' List the experiments of a model and their types
#'
#' List the experiments of a given model.
#'
#' @param file path to a gaml model file.
#'
#' @importFrom stringr str_match_all str_match regex str_detect
#' @importFrom  purrr map
#'
#' @examples
#' gaml_file <- system.file("examples", "sir.gaml", package = "rama")
#' show_experiment(gaml_file)
#'
#' @export
show_experiment <- function(file){
  if (!file.exists(file)) {
    stop(paste0("There is no file \"", file, "\"."))
  }

  gaml <- paste(readLines(file, warn = FALSE), collapse = "\n")
  exps <- str_match_all(gaml,
                        regex("\\nexperiment (.*?)\\{", dotall = T))[[1]][, 2]

  if (length(exps) == 0)
    stop(paste0("Model \"", file, "\" does not contain any experiment."))
  exps <- trimws(gsub("\\n+$", "", exps))
  exp_info <- purrr::map(exps, function(x){
    if (str_detect(x, "type")) {
      tmp <- cbind(str_match(x, ".*?(?=\\s+type?)"),
                   trimws(str_match(x, "type\\:(.*)"))[, 2])
      # if the experiment type contains other information (ex:"gui keep:true")
      if (grepl(":", tmp[1, 2])) {
        # remove group of character with ":" (ex:" keep:true")
        tmp[1, 2] <- gsub(" (.*)\\:(.*)", "", tmp[1, 2])
        tmp
      }
    } else {
      tmp <- cbind(x, "gui")
    }
    tmp
  })

  exp_info <- as.data.frame(do.call(rbind, lapply(exp_info, function(x) x)))
  names(exp_info) <- c("experiment", "type")
  exp_info$experiment <- as.character(exp_info$experiment)
  # test if there is special character in experiment name
  test_schar(exp_info$experiment)
  exp_info$type <- as.character(exp_info$type)
  return(exp_info)
}
