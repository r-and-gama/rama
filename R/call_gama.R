################################################################################
#' Run GAMA on a XML file
#'
#' From a XML file containing an experiment plan, send and run it in GAMA and
#' returns one XML file by simulation containing the output.
#'
#' @param experiment_plan an XML file containing the experiment
#' @param hpc numeric, number of cores
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
  if (isWindows() == FALSE) {
    output_display <- ">/dev/null"
  }

  gama_command <- system(
    paste0("java -jar \"", getOption("rama.startjar"), "\" -Xms",
           getOption("rama.Xms"), " -Xmx", getOption("rama.Xmx"),
           " -Djava.awt.headless=true org.eclipse.core.launcher.Main ",
           "-application msi.gama.headless.id4 -hpc ", hpc, " \"",
           parameter_xml_file, "\" \"", output_dir, "\"", output_display),
    ignore.stdout = F, ignore.stderr = T)

  if (gama_command > 0)
      stop(paste0("Gama fails to run your experiment \"",
                  expname(experiment_plan), "\"."))
  return(dir(path = output_dir,
             pattern = "[simulation-outputs[:digit:]+]\\.xml",
             full.names = TRUE))
}
