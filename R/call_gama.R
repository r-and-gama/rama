################################################################################
#' Run GAMA on a XML file
#'
#' From a XML file containing an experiment plan, send and run it in GAMA and
#' returns one XML file by simulation containing the output.
#'
#' @param parameter_xml_file path to an XML file containing an experiment.
#' @param hpc numeric, number of cores
#' @param output_dir path to saved the output of gama. If not specified, current
#'                   working directory will be used. If `output_dir` doesn't
#'                   exist, it will be created.
#'
#' @examples
#' parameter_xml_file <- system.file("examples", "sir.xml", package = "rama")
#' (outfiles <- call_gama(parameter_xml_file, hpc = 1, output_dir = tempdir()))
#'
#' @export
call_gama <- function(parameter_xml_file, hpc, output_dir = "") {
  if(output_dir == "")
    output_dir <- getwd()

  if(!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE)

  cat(paste0("Running experiment plan ... \n"))

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
      stop(paste0("Gama fails to run your experiment."))
  return(normalizePath(dir(path = output_dir,
             pattern = "[simulation-outputs[:digit:]+]\\.xml",
             full.names = TRUE)))
}
