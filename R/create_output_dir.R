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
  if (dir == ""){
    dir <- gsub("\"", "", expname(experiment_plan)) # name of experiment
  }

  i <- 0
  repeat {
    i <- i + 1
    out_dir <- paste0(wkdir, "/", dir, "_", i)
    if (!file.exists(out_dir)) break
  }
  dir.create(out_dir)
  out_dir
}
