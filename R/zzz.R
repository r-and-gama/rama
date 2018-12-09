# Returns the OS ---------------------------------------------------------------

get_os <- function() paste0(Sys.info()["sysname"])




# Gives distrib as a function of the OS ----------------------------------------

gama_remote_distrib <- function() {
  switch(get_os(),
         "Darwin" = paste0(options("rama.repo"), options("rama.default.gama.osx")),
         "Window" = "truc",   # to complete
         "linux"  = "bidule") # to complete
}




# Downloads gama ---------------------------------------------------------------

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
         "Darwin" = options("rama.default.gama.osx.appdir"),
         "Window" = options("rama.default.gama.win.appdir"),
         "linux" = options("rama.default.gama.linux.appdir")
         );
  paste0(export_dir,gama_app);
}




# ------------------------------------------------------------------------------

defpath <- function(path) {
  os <- paste0(Sys.info()["sysname"])
  # if we use OSX, the plugin is located in the Contents/eclipse sub directory of
  # gama otherwise it is at its root:
  subpath <- ifelse(os=="Darwin","/Contents/Eclipse","")
  gamapath <- paste0(path,subpath,"/plugins")
  plugins <- grep("org.eclipse.equinox.launcher_.*",dir(gamapath),value=T)
  options(rama.plugins = paste(paste0(gamapath,"/",plugins),collapse=":"))
  defaultjar <- paste0(gamapath,"/",plugins)
  options(rama.startjar=defaultjar)
  options(rama.Xmx="2048m")
  options(rama.Xms="512m")
}




# Interface to download GAMA if necessary --------------------------------------

download_gama_ui <- function() {
  repeat {
    answer <- toupper(readline("Do you want to download the last version of Gama? [y/n] "))
    if (answer[1] == "Y" | answer[1] == "N") break
    else print("Sorry, I din't understand... try again")
  }
  if (answer[1] == "Y") {
    gama_path <- download_gama()
    defpath(gama_path)
  }
}




# On attach --------------------------------------------------------------------

.onAttach <- function(...) {
  rama_workspace <- paste0(getwd(), "/workspace")
  options(rama.workspace                 = rama_workspace,
          rama.temp_dir                  = paste0(rama_workspace, "/temp"),
          rama.gama_dir                  = paste0(rama_workspace, "/gama"),
          rama.default.gama.win          = "/GAMA1.8_RC2_EmbeddedJDK_Win_64bits.zip",
          rama.default.gama.win.appdir   = "",
          rama.default.gama.osx          = "/GAMA1.8_RC2_EmbeddedJDK_MacOS.zip",
          rama.default.gama.osx.appdir   = "Gama.app",
          rama.default.gama.linux        = "/GAMA1.8_RC2_EmbeddedJDK_Linux_64bits.zip",
          rama.default.gama.linux.appdir = "",
          rama.repo                      = "http://51.255.46.42/releases")

#  packageStartupMessage("Welcome to rama v0.0.1!")
#  packageStartupMessage("WARNING: GAMA 1.7.0 needs to be installed on your machine.")
#  packageStartupMessage("Check www.gama-platform.org for installation instructions.")
#download_gama_ui();
#  defpath("/Users/choisy/Applications/Gama")
#  defpath("/Applications/Gama.app")


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
          rama.Xms                       = NULL)
}
