# Defines the GAMA repository --------------------------------------------------

gama_repo <- function(repo = NULL) {
  if (! is.null(repo)) options(rama.repo = repo)
}

isWindows <- function()
{
  return (Sys.info()["sysname"]=="Windows")
}

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

# download GAMA when necessary -------------------------------------------------
#' @export
setup_gama <- function() {
  repeat {
    print("Do you want to download the last version of Gama?")
    answer <- toupper(readline(" [Y]es/[N]o/[A]lready done."))
    if (answer[1] == "Y" | answer[1] == "N" | answer[1] == "A") break
    else print("Sorry, I din't understand... try again")
  }
  if (answer[1] == "Y") {
    gama_path <- download_gama()
    defpath(gama_path)
  }

    if (answer[1] == "A") {
    #   gama_path <- download_gama()
    #    defpath(gama_path)
  }
}
