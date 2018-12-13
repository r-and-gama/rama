# Environment Variables

set_environment_variables <- function()
{
  rama_workspace <- paste0(getwd(), "/workspace")
  options(rama.workspace                     = rama_workspace,
          rama.temp_dir                      = paste0(rama_workspace, "/temp"),
          rama.gama_dir                      = paste0(rama_workspace, "/gama"),
          rama.default.gama.win              = "/GAMA1.8_RC2_EmbeddedJDK_Win_64bits.zip",
          rama.default.gama.win.appdir       = "C:/programmes/Gama",
          rama.default.gama.win.zip.appdir   = "Gama",
          rama.default.gama.osx              = "/GAMA1.8_RC2_EmbeddedJDK_MacOS_zp.tgz",
          rama.default.gama.osx.appdir       = "/Applications/Gama.app",
          rama.default.gama.osx.zip.appdir   = "",
          rama.default.gama.linux            = "/GAMA1.8_RC2_EmbeddedJDK_Linux_64bits.zip",
          rama.default.gama.linux.appdir     = "/usr/local/Gama",
          rama.default.gama.linux.zip.appdir = "Gama",
          rama.repo                          = "http://51.255.46.42/releases",
          rama.gama.path                     = "UNKNOWN",
          rama.startjar                      = "UNKNOWN",
          rama.plugins                       = "UNKNOWN")
}

# Returns the OS ---------------------------------------------------------------

get_os <- function() paste0(Sys.info()["sysname"])


# Gives distrib as a function of the OS ----------------------------------------
#' In function of remote distribution of the OS returns the path
#' @noRd
gama_remote_distrib <- function() {
  switch(get_os(),
         "Darwin"  = paste0(options("rama.repo"), options("rama.default.gama.osx")),
         "Windows" = paste0(options("rama.repo"), options("rama.default.gama.win")),   # to complete C:\Program Files\
         "linux"   =  paste0(options("rama.repo"), options("rama.default.gama.linux"))) # to complete
}

# ------------------------------------------------------------------------------
#' In function of remote distribution of the OS returns the path
#' @noRd
gama_local_distrib_path <- function() {
  path <- switch(get_os(),
         "Darwin"  = options("rama.default.gama.osx.appdir"),
         "Windows" = options("rama.default.gama.win.appdir"),   # to complete C:\Program Files\
         "linux"  = options("rama.default.gama.linux.appdir")) # to complete
  unlist(path)
}

# -------------------------------------------

init_gama_path <- function(path) {
  os <- paste0(get_os())
  subpath <- ifelse(os=="Darwin","/Contents/Eclipse","")
  res <- paste0(path,subpath,"/plugins")
  ifelse(dir.exists(res),res,NA)
}

# -------------------------------------------

init_gama_jar <- function(path) {
  gamapath <- init_gama_path(path)
  if(is.na(gamapath))
    return(NA)
  plugins <- grep("org.eclipse.equinox.launcher_.*",dir(gamapath),value=T)
  res <- paste0(gamapath,"/",plugins)
  ifelse(file.exists(res),res,NA)
}


# ------------------------------------------------------------------------------

defpath <- function(path) {
  defaultjar <- init_gama_jar(path)
  if(is.na(defaultjar)) {
    stop("Gama configuration failed!")
  }
  else {
    options(rama.startjar=defaultjar,
            rama.Xmx="4096m",
            rama.Xms="512m",
            rama.gama.path = path)
    message("Gama configuration succeed!")
  }
}


# Interface to download GAMA if necessary --------------------------------------
#' Test if GAMA is intalled
#'
#' Test if GAMA is intalled and is correctly linked to the application Gama
#' platform
#'
#' @param path path to the application Gama platform
#'
#' @examples
#' \dontrun{
#' # For examples, for MacOs, it can be:
#' is_gama_installed(path = "/Applications/Gama.app")
#' }
#' @export
is_gama_installed <- function(path = unlist(options("rama.gama.path"))) {
  options("rama.startjar") != "UNKNOWN" | (dir.exists(path) & (!is.na(init_gama_jar(path))))
}

# On attach --------------------------------------------------------------------

.onAttach <- function(...) {
  set_environment_variables()
  packageStartupMessage("Welcome to rama v0.0.1!\n")
  packageStartupMessage("GAMA platform needs to be installed on your machine.")
  packageStartupMessage("See http://www.gama-platform.org for more instructions about GAMA.\n")
 # defpath("/Applications/Gama.app")
  pehaps_path <- gama_local_distrib_path()
  if(is_gama_installed(pehaps_path))
  {
    packageStartupMessage(paste0("-- note that GAMA platform was found at ",pehaps_path,"\n"))
    defpath(pehaps_path)
  }
  else
  {
    packageStartupMessage("WARNING: GAMA platform not found! Proceed to the setup")
    packageStartupMessage("-------: Use the command `setup_gama()` to setup or download GAMA")
  }
}




# On detach --------------------------------------------------------------------

.onDetach <- function(...) {
  options(rama.workspace                 = NULL,
          rama.temp_dir                  = NULL,
          rama.gama_dir                  = NULL,
          rama.default.gama.win          = NULL,
          rama.default.gama.win.appdir   = NULL,
          rama.default.gama.osx          = NULL,
          rama.default.gama.osx.appdir   = NULL,
          rama.default.gama.linux        = NULL,
          rama.default.gama.linux.appdir = NULL,
          rama.repo                      = NULL,
          rama.plugins                   = NULL,
          rama.startjar                  = NULL,
          rama.Xmx                       = NULL,
          rama.Xms                       = NULL,
          rama.gama.path                 = NULL)
}
