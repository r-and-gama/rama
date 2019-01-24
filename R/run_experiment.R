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
#'
#' @examples
#'df <-   data.frame(S0 = c(900, 800, 500), # this is a data frame of 3 lines
#'                   I0 = c(100, 200, 500),
#'                   R0 = 0,
#'                   beta = 1.5,
#'                   gamma = .15,
#'                   S = c(1,2,3),
#'                   I = c(2,4,6),
#'                   R =c(10,20,30),
#'                   nbiter = 1000,
#'                   seed = "123456789")
#'
#'exp0 <- experiment(
#'  df,
#'  parameters = c("S0","I0","R0","beta","gamma"),
#'  obsrates  = c("S", "I", "R"),
#'  tmax = "nbiter",
#'  seed = "seed",
#'  experiment = "sir",
#'  model = system.file("examples", "sir.gaml", package = "rama"),
#'  dir = "testsir"
#')
#'
#'
#'exp0
#'otp <- run_experiment(exp0)
#'str(otp)
#'newoutput <- realexp(otp,exp0)
#'str(newoutput)
#'@noRd
realexp <- function(output, exp){

  newoutput <- list()
  for (j in 1:nrow(exp)) {

    curexp <- exp
    cursimulnum  <- j
    # In the current experiment object,
    # cursimul gives the line of the simulation
    curoutput    <- output[cursimulnum][[1]]
    # build the vector of observed variables
    curobs <- attributes(curoutput)$names[-1]
    # build the index of the variables whose rate is computed
    curobsidx <- sapply(X = curobs, function(x) which(colnames(curoutput) == x))
    # build the vector of text corresponding of the attributes of rates
    observed <- paste("r_", curobs, sep = "")
    # build the index of the variables whose rate is computed
    ratesidx <- sapply(X = observed, function(x) which(colnames(curexp) == x))
    # retrieve the value of the rates
    ratesval <- (as.data.frame(curexp)[cursimulnum, ratesidx])

    curoutput    <- output[cursimulnum][[1]]
    for (i in 1:length(as.vector(curobsidx))) {
      freq <- as.integer(ratesval[i])
      curvalue <- curoutput[, curobsidx[i]]
      max <- length(curvalue)
      if (freq != 1) {
        curvalue[sapply(X = 1:max, function(x) (x %% freq != 1))] <- NA
        curoutput[, curobsidx[i]] <- curvalue
      }
    }
    if (length(newoutput) == 0) {
      newoutput <- list(curoutput)
    }
    else {
      newoutput <- c(newoutput, list(curoutput))
    }
  }
  return(newoutput)
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
#' simulation and also the simulation output of Gama in XML file stored in
#' `output_dir` (created by \code{\link[rama]{create_output_dir}})
#'
#' @param exp an XML file containing the experiment
#' @param hpc numeric
#' @param output_dir path to saved the output of gama
#' @param parameter_xml_file name of XML parameter file. This file is created
#'                           in the working directory of `exp`. If not
#'                           specified, name of `exp` is used.
#'
#' @examples
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
    output_dir <- create_output_dir(exp, output_dir)

  # generate xml file from exp
    parameter_xml_file <- save_to_gama(exp, parameter_xml_file)

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
