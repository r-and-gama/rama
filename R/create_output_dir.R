################################################################################
#' Create output folder
#'
#' Creates output folder named by the `experiment` name. If multiple
#' `experiment` are called with the same name, a numeric is append to avoid
#' confusion.
#'
#' @param exp an object of class `experiment`
#' @param dir name of directory to save the output of gama in the experiment
#'            output directory. If not specified, experiment name will be used.
#'
#' @example inst/examples/create_output_dir.R
#' @export
create_output_dir <-  function(exp, dir = "") {
  wkdir <- output_dir(exp)
  if (dir == ""){
    dir <- gsub("\"", "", name(exp)) # name of experiment
  }
  out_dir <- paste0(wkdir, "/", dir)
  if (dir.exists(out_dir)) {
    i <- 0
    repeat {
      i <- i + 1
      out_dir <- paste0(out_dir, "_", i)
      if (!file.exists(out_dir)) break
    }
  }

  dir.create(out_dir, recursive = TRUE)
  normalizePath(out_dir)
}
