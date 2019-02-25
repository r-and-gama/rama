# realexp --------------------------------------------------------------
#' Restore the correct values of an output
#'
#' It puts a NA whenever an observation was in fact not made in GAMA.
#'
#' From the list of frequencies of each simulation of the experiment
#' the values incorrectly associated to the last value observed are reset to NA.
#'
#' @param output An object of class \code{list}.
#' @param exp An object of class \code{experiment}.
#'
#' @return Returns a list of dataframes, one for each experiment.
#' @noRd
realexp <- function(output, exp) {
  op <- obs_rates(exp)
  mapply(function(nbrow, obsper, df) {
           xs <- lapply(obsper, function(by) setdiff(1:nbrow, seq(1, nbrow, by)))
           ys <- sapply(names(xs), grep, names(df))
           df[, ys] <- mapply(replace, df[, ys], xs, NA)
           return(df)
         },
         lapply(output, nrow),
         lapply(as.data.frame(t(op)), setNames, names(op)),
         output, SIMPLIFY = FALSE)
}

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

  new_name <- as.vector(attr(exp, "dic_g2r")[lst_name])
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
#' simulation.
#'
#' @param exp an XML file containing the experiment
#' @param hpc numeric
#'
#' @example inst/examples/run_experiment.R
#' @export
run_experiment <- function(exp, hpc = 1) {

  if (!is.experiment(exp)) {
    stop("The argument `exp` is not an `experiment` object.")
  }

  # make output directory
    output_dir <- tempfile(tmpdir = tempdir())
    dir.create(output_dir)
  # generate xml file from exp

    parameter_xml_file <- save_to_gama(validate_experiment(exp),
                                       filename = NULL,
                                       path = output_dir)
  # run all the experiments
    outfiles <- call_gama(parameter_xml_file, hpc, output_dir)

  # get variables names
  vars <- names(exp)[grep("r_", names(exp))]
  vars <- substring(vars, 3)
  vars <- as.vector(attr(exp, "dic_g2r")[vars])

  # retrieve all the variables of all the experiments:
  out <- lapply(outfiles, retrieve_results, exp)

  # Correct NA observations

  out <- realexp(out, exp)

  # deleting the "workspace" folder:
  unlink("workspace", T, T)
  # return output:
  out
}
