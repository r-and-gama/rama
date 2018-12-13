# Retrieve_results  ------------------------------------------------------------
#' Get results from XML to data frame
#'
#' get results from output files for all variables (r_)
#'
#' The `experiment` object used to create the outfile is called to match the
#' name and parameters.
#'
#' @param outfile XML file to parse
#' @param experiment_plan object of class `experiment` used to create the
#' outfile
#'
#' @noRd
#' @importFrom XML xmlToDataFrame
retrieve_results <- function(outfile, experiment_plan) {
  # Extract a data frame
  tmp <- XML::xmlToDataFrame(XML::xmlParse(outfile), stringsAsFactors = F)
  # Extract names of the variable
  lst_name <- unlist(XML::xmlToList(XML::xmlParse(outfile)))
  lst_name <- lst_name[which(grepl("name", names(lst_name)))]
  lst_name <- unique(lst_name)

  # Tidy the output
  tmp2 <- lapply(seq_len(dim(tmp)[2]), function(x) {
    suppressWarnings(if (is.numeric(as.numeric(tmp[, x]))) {
      tmp[, x] <- as.numeric(tmp[, x]) } else { tmp[, x] })
  })
  tmp <- as.data.frame(setNames(tmp2, lst_name))

  new_name <- as.vector(attr(experiment_plan, "dic_v")[lst_name])
  names(tmp) <- new_name
  tmp$Step <- c(0:(dim(tmp)[1] - 1))
  tmp <- tmp[, c("Step", new_name)]
  attr(tmp, "path") <- outfile
  tmp
}

################################################################################
#' Create output folder
#'
#' Creates output folder named by the `experiment` name. If multiple
#' `experiment` are called with the same name, a numeric is append to the same
#' to avoid confusion.
#'
#' @param experiment_plan an XML file containing the experiment
#' @param dir path to saved the output of gama
#'
#' @examples
#' #load experiment
#' gaml_file <- system.file("examples", "sir.gaml", package = "rama")
#' exp1 <- load_experiment("sir", gaml_file, "sir")
#'
#' #create path to folder in working direction
#' output_dir <- create_output_dir(exp1, "")
#' parameter_xml_file <-  paste0(expname(exp1), ".xml")
#' @export
create_output_dir <- function(experiment_plan, dir = "") {
  wkdir <- get_wkdir(experiment_plan)
  if(dir == ""){
    dir <- gsub("\"","", expname(experiment_plan)) # name of experiment
  }

  i <- 0
  repeat {
    i <- i + 1
    out_dir <- paste0(wkdir, "/", dir, "_", i)
    if(!file.exists(out_dir)) break
  }
  dir.create(out_dir)
  out_dir
}

################################################################################
#' Run GAMA on a XML file
#'
#' From a XML file containing an experiment plan, send and run it in GAMA and
#' returns one XML file by simulation containing the output.
#'
#' @param experiment_plan an XML file containing the experiment
#' @param hpc numeric
#' @param output_dir path to saved the output of gama
#' @param parameter_xml_file path to folder containing the xml file
#'
#' @examples
#' #load experiment
#' gaml_file <- system.file("examples", "sir.gaml", package = "rama")
#' exp1 <- load_experiment("sir", gaml_file, "sir")
#'
#' #create path to folder in working direction
#' output_dir <- create_output_dir(exp1, "")
#' parameter_xml_file <-  paste0(expname(exp1), ".xml")
#'
#' # create the XML input file
#' parameter_xml_file <- save_to_gama(exp1,
#'    paste0(output_dir, "/", parameter_xml_file))
#'
#' # Call gama
#' outfiles <- call_gama(experiment_plan, hpc = 1, output_dir,
#'    parameter_xml_file)
#'
#' @export
call_gama <- function(experiment_plan, hpc, output_dir, parameter_xml_file) {
  cat(paste0("Running experiment plan ..."))

  output_display <- ""
  if(isWindows()==FALSE)
  {
    output_display <-">/dev/null"
  }

  gama_command <- system(paste0("java -jar \"",getOption("rama.startjar"),"\" -Xms",
                              getOption("rama.Xms")," -Xmx",getOption("rama.Xmx"),
                              " -Djava.awt.headless=true org.eclipse.core.launcher.Main ",
                              "-application msi.gama.headless.id4 -hpc ",hpc," \"",
                              parameter_xml_file,"\" \"",output_dir,"\"", output_display),
                       ignore.stdout=F,ignore.stderr=T)

  if(gama_command>0) return(-1)
  return(dir(path = output_dir, pattern = "[simulation-outputs[:digit:]+]\\.xml",  full.names = TRUE))
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
#' @param experiment_plan an XML file containing the experiment
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
run_experiment <- function(experiment_plan, hpc = 1, output_dir = "", parameter_xml_file = "") {

  # make output directory
  if(output_dir=="")
    output_dir <- create_output_dir(experiment_plan, output_dir)

  # generate xml file from experiment_plan
  if(parameter_xml_file == "")
    parameter_xml_file <-  paste0(expname(experiment_plan), ".xml")
  parameter_xml_file <- save_to_gama(experiment_plan, paste0(output_dir, "/", parameter_xml_file))

  # run all the experiments
  outfiles <- call_gama(experiment_plan, hpc, output_dir, parameter_xml_file)

  # get variables names
  vars <- names(experiment_plan)[grep("r_", names(experiment_plan))]
  vars <- substring(vars, 3)
  vars <- as.vector(attr(experiment_plan, "dic_v")[vars])

  # retrieve all the variables of all the experiments:
  out <- lapply(outfiles, retrieve_results, experiment_plan)

  # deleting the "workspace" folder:
  unlink("workspace", T, T)
  # return output:
  out
}

## To be revised ---------------------------------------------------------------
## NOT CALLED
#getoutputs <- function(out, outputs)
#{
#  thenames <- unname(sapply(out2[[1]],function(x)x$.attrs))
#  sel <- which(thenames %in% outputs)
#  out3 <- lapply(out2,function(x) x[sel])
#  out3 <- out3[-length(out3)]
#  trc <- c(t(sapply(out3,function(y)sapply(y,function(x)x$text)))) #as.data.frame(t(sapply(out3,function(y)sapply(y,function(x)x$text))),stringsAsFactors=F)
#  steps <-c(as.numeric(t(sapply(out[-length(out)],function(x)x$.attrs["id"]))))
#  output<- as.data.frame(steps,trc)
#  output
#}

### mercredi 12 avril 2017

# returns the name of an available subfolder:
# createmodelparameterfilename <- function(experimentname) {
#   outdirectory <- createworkingdirectory()
#   i <- 0
#   repeat {
#     i <- i + 1
#     outfile <- paste0(outdirectory,"/",experimentname,"_",i,".xml")
#     if(!file.exists(outfile)) break
#   }
#   outfile
# }
################################################################################

cleaning <- function() unlink("workgamar",T,T)
