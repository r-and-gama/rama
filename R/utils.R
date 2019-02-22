read_gaml_experiment <- function(exp, model) {
  tmp <- tempfile(fileext = ".xml")
  system(paste0("java -jar ", getOption("rama.startjar"),
                " -Xms", getOption("rama.Xms"),
                " -Xmx", getOption("rama.Xmx"),
                " -Djava.awt.headless=true org.eclipse.core.launcher.Main",
                " -application msi.gama.headless.id4 -xml ",
                exp, " '", model, "' ", tmp, " > /dev/null"),
         ignore.stdout = TRUE, ignore.stderr = TRUE)
  unlink("workspace", TRUE, TRUE)

  if (file.exists(tmp)) return(XML::xmlToList(XML::xmlParse(tmp))$Simulation)
  stop(paste0("Gama fails to read your experiment"))
}


# test special characters ------------------------------------------------------
test_schar <- function(x) {
  if (any(grepl("[\\&|\\<|\\>|\\']", x))) {
    stop(paste0("The rama package does not support the specials characters `<`",
                ", `>`, `&` and `'` in parameters,",
                " outputs and experiments names."))
  }
}



# Check if a requested experiment is valid -------------------------------------
# For a requested experiment to be valid, we need
# * the name to exist in the file;
# * the experiment to be of type "GUI".
check_experiment <- function(exp, model) {
  model_info <- model$info
    # check if the requested experiment is present in the file:
  if (!exp %in% model_info$.attr["experiment"])
    stop(paste0("There is no experiment named \"", exp, "\" in ",
                basename(model_info$.attr["sourcePath"])))
  invisible(0)
}



# make_dictionary --------------------------------------------------------------
#' @importFrom stats setNames
make_dictionary <- function(x) {
  dic <- gsub("[[:space:]]|[[:punct:]]", "_", x)
  dic <- gsub("_+", "_", dic)
  setNames(dic, x)
}



# Make working directory -------------------------------------------------------
# Uses full path "dir" if specified. If only name specified (i.e. without any
# "/"), use current directory. If not specified, use default name.
make_wkdir <- function(model, dir = "") {

  if (dir == "") {
    # get model name from gaml file
    dir <- gsub(".gaml", "", basename(model))
    message(cat("Using default directory name \"", dir,
                   "\" in current directory \"", getwd(), "\".", sep = ""))
  }

  if (dir.exists(dir)) {
    i <- 0
    repeat {
      i <- i + 1
      wk_dir <- paste0(dir, "_", i)
      if (!file.exists(wk_dir)) break
    }
  } else {
    wk_dir <-  dir
  }

  dir.create(wk_dir, recursive = TRUE)
  message(cat("Simulations results will be saved in \"", wk_dir, "\".",
              sep = ""))
  normalizePath(wk_dir)
}



# Defines the GAMA repository --------------------------------------------------
gama_repo <- function(repo = NULL) {
  if (! is.null(repo)) options(rama.repo = repo)
}

# Test if operating system is Windows ------------------------------------------
isWindows <- function() {
  return (Sys.info()["sysname"] == "Windows")
}

# Returns the OS ---------------------------------------------------------------
get_os <- function(){
  os <- paste0(Sys.info()["sysname"])
  if (is.null(os)){
    if (grepl("^darwin", R.version$os))
      os <- ""
    if (grepl("linux-gnu", R.version$os))
      os <- "Linux"
  }
  os
}

# Gives distrib as a function of the OS ----------------------------------------
#' In function of remote distribution of the OS returns the path
#' @noRd
gama_remote_distrib <- function() {
  switch(get_os(),
         "Darwin"  = paste0(options("rama.repo"),
                            options("rama.default.gama.osx")),
         "Windows" = paste0(options("rama.repo"),
                            options("rama.default.gama.win")),
         # to complete C:\Program Files\
         "Linux"   =  paste0(options("rama.repo"),
                             options("rama.default.gama.linux")))
  # to complete
}

# Downloads gama ---------------------------------------------------------------
#' @importFrom utils download.file unzip untar
#' @importFrom downloader download
download_gama <- function() {
  distrib <- gama_remote_distrib()
  expDir  <- gama_local_distrib_path();
  path <- paste0(options("rama.temp_dir"), "/")
  path_out <- paste0(path, "out")
  path_dist <- paste0(path, "out/", basename(expDir))
  path_test <- dirname(expDir)
  distrib_file <- paste0(path, "downloaded_gama.tgz")
  if (! dir.exists(path)) dir.create(path, recursive = TRUE)

  if (nchar(options("rama.default.gama.osx.zip.appdir")) > 0) {
    path_out <- paste0(path, options("rama.default.gama.osx.zip.appdir"))
    path_test <- paste0(path_test, "/",
                        options("rama.default.gama.osx.zip.appdir"))
  }
  download(distrib, distrib_file,  mode = "wb")
  untar(distrib_file, exdir = path_test, compressed = "gzip" )
  gama_app <- switch(get_os(),
                     "Darwin" = options("rama.default.gama.osx.appdir"),
                     "Window" = options("rama.default.gama.win.appdir"),
                     "linux" = options("rama.default.gama.linux.appdir"))
  expDir
}

# Setup GAMA UI ----------------------------------------------------------------
setup_gama_ui <- function() {
  defaultjar <- ""
  repeat{
    message(cat("Give the path of Gama platform or [Q]uit:"))
    answer <- readline()
    if (answer[1] == "Q" | answer[1] == "q" ) return(NA)
    defaultjar <- is_gama_installed(answer)
    if (defaultjar) break
    else {
      warning("Gama is not found at the specified location")
      warning("Please give the correct location")
    }
  }
  answer
}



# download GAMA when necessary -------------------------------------------------
#' Download GAMA and configure
#'
#' Prompt the user to download GAMA if he wish to and then configures
#' \code{rama}, linking it to a GAMA engine installed on the system.
#'
#' @param path path to the applicatation Gama Platform
#'
#' @examples
#' \dontrun{
#' # For interactive usage:
#' setup_gama()
#'
#' # If input a path, the function will not be interactive:
#' setup_gama(getwd())
#' }
#'
#' @export
setup_gama <- function(path = NA) {
  if (!is.na(path)) {
    defpath(path)
    return(NA)
  }
  if (is_gama_installed()) {
    message(cat("Gama is already installed, do you want to setup a new one? "))
    answer <- toupper(readline("[Y]es/[N]"))
    if (answer[1] == "N") return(NA)
  }

  repeat {
    message(cat("Do you want to download the last version of Gama? "))
    answer <- toupper(readline(" [Y]es/[N] or [K]eep my current version."))
    if (answer[1] == "Y" | answer[1] == "N" | answer[1] == "K") break
    else print("Sorry, I din't understand... try again")
  }
  if (answer[1] == "Y") {
    gama_path <- download_gama()
    defpath(gama_path)
  }
    if (answer[1] == "K") {
       gama_path <- setup_gama_ui()
       if (is.na(gama_path)) return(NA)
       defpath(gama_path)
  }
}
