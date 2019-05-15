#' List the experiments of a model and their types
#'
#' List the experiments of a given model.
#'
#' @param file Path to a \code{.gaml} file.
#'
#' @example inst/examples/list_experiments.R
#' @export
list_experiments <- function(file){
  if (!file.exists(file)) {
    stop(paste0("File \"", file, "\" does not exist."))
  }

  gaml <- paste(readLines(file, warn = FALSE), collapse = "\n")
  exp_info <- regexpr("\\nexperiment (.*?)\\{", gaml)
  exp_info <- substr(gaml, exp_info, exp_info + attr(exp_info, "match.length"))
  exps <- gsub("\n|experiment|\\{", "", exp_info)

 # exps <- str_match_all(gaml,
  #                      regex("\\nexperiment (.*?)\\{", dotall = TRUE))[[1:2]]

  if (length(exps) < 1)
    stop(paste0("File \"", file, "\" does not contain any experiment."))
  exps <- trimws(gsub("\\n+$", "", exps))
  exp_info <- lapply(exps, function(x) {
    if (grepl("type", x)) {
      tmp <- cbind(trimws(substr(x, 1, regexpr("type", x) - 1)),
        #str_match(x, ".*?(?=\\s+type?)"),
                   trimws(substr(x, regexpr("type", x) + 5, nchar(x))))
      #str_match(x, "type\\:(.*)"))[, 2])
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

  exp_info <- as.data.frame(do.call(rbind, exp_info), stringsAsFactors = FALSE)
  names(exp_info) <- c("experiment", "type")
  # test if there is special character in experiment name
  test_schar(exp_info$experiment)
  exp_info
}
