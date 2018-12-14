# Retrieve_results  ------------------------------------------------------------
#' Get results from XML to data frame
#'
#' get results from output files for all variables (r_)
#'
#' The `experiment` object used to create the outfile is called to match the
#' name and parameters.
#'
#' @param outfile XML file to parse
#' @param exp object of class `experiment` used to create the
#' outfile
#'
#' @importFrom XML xmlToDataFrame
#' @noRd
retrieve_results <- function(outfile, exp) {
  # Extract a data frame
  tmp <- XML::xmlToDataFrame(XML::xmlParse(outfile), stringsAsFactors = F)
  # Extract names of the variable
  lst_name <- unlist(XML::xmlToList(XML::xmlParse(outfile)))
  lst_name <- lst_name[which(grepl("name", names(lst_name)))]
  lst_name <- unique(lst_name)

  # Tidy the output
  tmp2 <- lapply(seq_len(dim(tmp)[2]), function(x) {
    suppressWarnings(if (is.numeric(as.numeric(tmp[, x]))) {
      tmp[, x] <- as.numeric(tmp[, x])
      } else {
        tmp[, x]
        })
  })
  tmp <- as.data.frame(setNames(tmp2, lst_name))

  new_name <- as.vector(attr(exp, "dic_rev")[lst_name])
  names(tmp) <- new_name
  tmp$Step <- c(0:(dim(tmp)[1] - 1))
  tmp <- tmp[, c("Step", new_name)]
  attr(tmp, "path") <- outfile
  tmp
}


################################################################################
#' Run an experiment
#'
#' From a `experiment` object, run an experiment by creating an XML file of the
#' experiment (\code{\link[rama]{save_to_gama}}) and by calling gama
#' (\code{\link[rama]{call_gama}}) and returns a list of data frame, one by
#' simulation and also the simulation output of Gama in XML file stored in
#' `output_dir` (created by \code{\link[rama]{create_output_dir}})
#'
#' @param exp an XML file containing the experiment
#' @param hpc numeric
#' @param output_dir path to saved the output of gama
#' @param parameter_xml_file path to folder containing the xml file
#'
#' #' @examples
#' #load experiment
#' gaml_file <- system.file("examples", "sir.gaml", package = "rama")
#' exp1 <- load_experiment("sir", gaml_file, "sir")
#' # run experiment
#' out <- run_experiment(exp1)
#' @export
run_experiment <- function(exp, hpc = 1, output_dir = "",
                           parameter_xml_file = "") {

  if (!is.experiment(exp)) {
    stop("The argument `exp` is not an `experiment` object.")
  }

  # make output directory
  if (output_dir == "")
    output_dir <- create_output_dir(exp, output_dir)

  # generate xml file from exp
  if (parameter_xml_file == "")
    parameter_xml_file <-  paste0(expname(exp), ".xml")
  parameter_xml_file <- save_to_gama(exp,
                                     paste0(output_dir, "/",
                                            parameter_xml_file))

  # run all the experiments
  outfiles <- call_gama(exp, hpc, output_dir, parameter_xml_file)

  # get variables names
  vars <- names(exp)[grep("r_", names(exp))]
  vars <- substring(vars, 3)
  vars <- as.vector(attr(exp, "dic")[vars])

  # retrieve all the variables of all the experiments:
  out <- lapply(outfiles, retrieve_results, exp)

  # deleting the "workspace" folder:
  unlink("workspace", T, T)
  # return output:
  out
}

