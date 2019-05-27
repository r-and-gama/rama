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
    ys <- sapply(paste0("^", names(xs), "$"), grep, names(df))
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
#' Create output folder
#'
#' @param dir directory to create the folder
#' @noRd
create_outdir <- function(dir) {
  dir.create(dir)
  input <- paste0(dir, "/input")
  dir.create(input)
  output <- paste0(dir, "/output")
  dir.create(output)
}


################################################################################
#' Run an experiment
#'
#' From an \code{experiment} object, run an experiment by creating an XML file
#' of the experiment (\code{\link[rama]{save_to_gama}}) and by calling gama
#' (\code{\link[rama]{call_gama}}) and appends the results to the object
#' \code{experiment} inputted in the column \code{output} (one by simulation).
#'
#' When the argument \code{save} is equal to \code{TRUE}, a folder with the name
#' of the experiment of the object \code{exp} is created. The folder contains
#' two folder: \code{output} containing the result in XML and \code{input}
#' containing the model file associated in GAML and the XML associated to the
#' object `exp` inputted in the function. \cr\cr
#' If the arguments \code{display} & \code{save} are equal to \code{TRUE}, a
#' folder \code{images} is add into the folder \code{output}, and contained
#' the display output from GAMA.
#'
#' @param exp an XML file containing the experiment.
#' @param hpc number of threads used by GAMA to run the experiment.
#' @param save save the outputs to disk or not, default = FALSE.
#' @param path directory to save the outputs, default = NULL.
#'             If \code{save = TRUE} and \code{path} is not
#'             specified, current working directory is used to save the outputs.
#' @param display output images are saved or not, default = FALSE.
#' @param append append outputs to experiment, default = TRUE. It is not possible
#'                to set both \code{add_exp} and `save` as \code{FALSE}.
#'
#' @example inst/examples/run_experiment.R
#' @export
run_experiment <- function(exp, hpc = 1, save = FALSE, path = NULL,
                           display = FALSE, append = TRUE) {

  if(all(ncol(parameters(exp)) == 0, ncol(obs_rates(exp)) == 0))
    return(exp)
  if (!is.experiment(exp))
    stop("The argument \"exp\" is not an object of class \"experiment\".")

  # after-run operations
  if (isFALSE(save) && isFALSE(append))
    stop("Outputs need to be saved either on disk or in experiment object.")

  # make output directory
  output_dir <- tempfile(tmpdir = tempdir())
  dir.create(output_dir)

  # generate xml file from exp
  parameter_xml_file <- save_to_gama(validate_experiment(exp),
                                     filename = NULL, path = output_dir)
  # run all the experiments
  outfiles <- call_gama(parameter_xml_file, hpc, output_dir)

  # retrieve all the variables of all the experiments:
  out <- lapply(outfiles, retrieve_results, exp)

  # Correct NA observations
  out <- realexp(out, exp)

  # Save input and output in path
  if (isTRUE(save)) {

    if (is.null(path)) {
      path <- getwd()
      message(paste0("Outputs are saved to \"", path, "\" by default.", sep = ""))
    }
    dir <- paste0(path, "/", name(exp))

    i <- 0
    while (file.exists(dir)) {
      i <- i + 1
      dir <- paste0(path, "/", name(exp), "_", i)
    }
    message(paste0("Outputs are saved to \"", dir, "\"."))
    create_outdir(dir)
    file.copy(parameter_xml_file, paste0(dir, "/input"))
    file.copy(model(exp)$path, paste0(dir, "/input"))
    file.copy(outfiles, paste0(dir, "/output"))

    if (isTRUE(display)) {
      images <- paste0(dir, "/output/images")
      dir.create(images)
      if (file.exists(paste0(output_dir, "/images")))
        file.copy(paste0(output_dir, "/images"), images, recursive = TRUE)
    }
  }

  if (isTRUE(append)) {
    old_attr <- attributes(exp)
    exp <- transform(exp, output = out)
    attributes(exp) <- old_attr
  }


  # return experiment:
  exp
}
