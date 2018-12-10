# Environment Variables

set_environment_variables <- function()
{
  rama_workspace <- paste0(getwd(), "/workspace")
  options(rama.workspace                     = rama_workspace,
          rama.temp_dir                      = paste0(rama_workspace, "/temp"),
          rama.gama_dir                      = paste0(rama_workspace, "/gama"),
          rama.default.gama.win              = "/GAMA1.8_RC2_EmbeddedJDK_Win_64bits.zip",
          rama.default.gama.win.appdir       = "C:/Program Files/Gama",
          rama.default.gama.win.zip.appdir   = "",
          rama.default.gama.osx              = "/GAMA1.8_RC2_EmbeddedJDK_MacOS.zip",
          rama.default.gama.osx.appdir       = "/Applications/Gama.app",
          rama.default.gama.osx.zip.appdir   = "Gama.app",
          rama.default.gama.linux            = "/GAMA1.8_RC2_EmbeddedJDK_Linux_64bits.zip",
          rama.default.gama.linux.appdir     = "",
          rama.default.gama.linux.zip.appdir = "",
          rama.repo                          = "http://51.255.46.42/releases",
          rama.gama.path                     = "UNKNOWN",
          rama.startjar                      = "UNKNOWN",
          rama.plugins                       = "UNKNOWN")
}

# Returns the OS ---------------------------------------------------------------

get_os <- function() paste0(Sys.info()["sysname"])


# Gives distrib as a function of the OS ----------------------------------------

gama_remote_distrib <- function() {
  switch(get_os(),
         "Darwin"  = paste0(options("rama.repo"), options("rama.default.gama.osx")),
         "Windows" = paste0(options("rama.repo"), options("rama.default.gama.win")),   # to complete C:\Program Files\
         "linux"  = "bidule") # to complete
}

# ------------------------------------------------------------------------------

gama_local_distrib_path <- function() {
  path <- switch(get_os(),
         "Darwin"  = options("rama.default.gama.osx.appdir"),
         "Windows" = options("rama.default.gama.win.appdir"),   # to complete C:\Program Files\
         "linux"  = options("rama.default.gama.linux.appdir")) # to complete
  unlist(path)
}



# Downloads gama ---------------------------------------------------------------


#' @importFrom utils download.file unzip
download_gama <- function() {
  distrib <- gama_remote_distrib()
  path <- paste0(options("rama.temp_dir"), "/")
  distrib_file <- paste0(path, "downloaded_gama.zip")
  export_dir <-  paste0(options("rama.gama_dir"), "/")
  if (! dir.exists(path)) dir.create(path, recursive = TRUE)
  download.file(distrib, distrib_file, quiet = FALSE, mode = "wb", cacheOK = TRUE)
  unzip(distrib_file, exdir = export_dir )
  file.remove(distrib_file)
  gama_app <- switch(get_os(),
         "Darwin" = options("rama.default.gama.osx.zip.appdir"),
         "Window" = options("rama.default.gama.win.zip.appdir"),
         "linux" = options("rama.default.gama.linux.zip.appdir")
         );
  paste0(export_dir,gama_app);
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
            rama.Xmx="2048m",
            rama.Xms="512m",
            rama.gama.path = path)
    message("Gama configuration succeed!")
  }
}


setup_gama_ui <- function() {
  print("Give the path of Gama platform :")
  answer <- toupper(readline())
}


# Interface to download GAMA if necessary --------------------------------------
#' @export
is_gama_installed <- function(path = unlist(options("rama.gama.path"))) {
  dir.exists(path) & (!is.na(init_gama_jar(path)))
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
