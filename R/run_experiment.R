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
#' @param exp an XML file containing the experiment.
#' @param hpc number of threads used by GAMA to run the experiment.
#' @param save save the outputs to disk or not, default = FALSE.
#' @param path directory to save the outputs, default = NULL.
#'             If `save = TRUE` and `path` is not
#'             specified, current working directory is used to save the outputs.
#' @param display output images are saved or not, default = FALSE.
#' @param append append outputs to experiment, default = TRUE. It is not possible
#'                to set both `add_exp` and `save` as `FALSE`.
#'
#' @example inst/examples/run_experiment.R
#' @export
run_experiment <- function(exp, hpc = 1, save = FALSE, path = NULL,
                          display = FALSE, append = TRUE) {

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

  # after-run operations
  if(isFALSE(save) && isFALSE(append))
    stop(paste0("Outputs needs to be saved either in disk or in experiment object."))

  if(isTRUE(save)){
    if(is.null(path)){
      path <-  paste0(getwd(), "/", name(exp))
      warning(paste0("Outputs are saved to '", path, "' by default."))
    }
    dir <- path
    if(file.exists(path)) {
      i <- 0
      repeat {
        i <- i + 1
        dir <- paste0(path, "_", i)
        if (!file.exists(dir)) break
      }
      warning(paste0("'", path, "' already exists. Outputs are saved in '",
                     dir, "'."))
    }
    dir.create(dir)
    input <- paste0(dir, "/input")
    dir.create(input)
    output <- paste0(dir, "/output")
    dir.create(output)
    file.copy(parameter_xml_file, input)
    file.copy(model(exp)$path, input)
    file.copy(outfiles, output)
  }

  if(isTRUE(display)){
    images <- paste0(dir, "/images")
    dir.create(images)
    if(file.exists(paste0(output_dir, "/images")))
      file.copy(paste0(output_dir, "/images"), images, recursive = TRUE)
  }

  if(isTRUE(append))
  # to do

 # return experiment:
  exp
}
