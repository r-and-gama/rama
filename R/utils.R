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
  untar(distrib_file, exdir = path )

  zipP <- paste0(path, "GAMA.zip")
  print(zipP)

  unzip(zipP, exdir = path_test,  overwrite = TRUE)
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
    message("Give the path of Gama platform or [Q]uit:")
    answer <- readline()
    if (answer[1] == "Q" | answer[1] == "q" ) return(NA)
    defaultjar <- is_gama_installed(answer)
    if (defaultjar) break
    else
    {
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
    message("Gama is already installed, do you want to setup a new one ? ")
    answer <- toupper(readline("[Y]es/[N]"))
    if(answer[1] == "N") return(NA)
  }

  repeat {
    message("Do you want to download the last version of Gama?")
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
       if(is.na(gama_path)) return(NA)
       defpath(gama_path)
  }
}
