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
  expDir  <- gama_local_distrib_path();
  path <- paste0(options("rama.temp_dir"), "/")
  path_out <- paste0(path, "out")
  path_dist <- paste0(path, "out/",basename(expDir))
  path_test <- dirname(expDir)
  distrib_file <- paste0(path, "downloaded_gama.tgz")
  if (! dir.exists(path)) dir.create(path, recursive = TRUE)

  if(nchar(options("rama.default.gama.osx.zip.appdir"))>0){
    path_out <- paste0(path, options("rama.default.gama.osx.zip.appdir"))
    path_test <- paste0(path_test,"/", options("rama.default.gama.osx.zip.appdir"))
  }

#  download.file(distrib, distrib_file,  quiet = TRUE, mode = "wb", cacheOK = TRUE)
  download(distrib, distrib_file,  mode = "wb")
 # download(distrib, "/tmp/truc.tgz",  mode = "wb")


  untar(distrib_file, exdir = path_test )
#  unz?(distrib_file, exdir = path_test,  overwrite = TRUE, unzip = "unzip" )

  # dir.create(expDir)
  #file.rename(path_dist,dirname(expDir)) #, recursive = TRUE,copy.mode = "copy")
  #file.rename(path_dist,dirname(expDir)) #, recursive = TRUE,copy.mode = "copy")
  # remove(distrib_file)
  #unlink(path_out, recursive = TRUE)
  gama_app <- switch(get_os(),
                     "Darwin" = options("rama.default.gama.osx.appdir"),
                     "Window" = options("rama.default.gama.win.appdir"),
                     "linux" = options("rama.default.gama.linux.appdir")
  );
  expDir
}


setup_gama_ui <- function() {
  defaultjar <- ""
  repeat{
    message("Give the path of Gama platform :")
    answer <- readline()
    defaultjar <- is_gama_installed(answer)
    if(defaultjar) break
    else
    {
      warning("Gama is not found at the specified location")
      warning("Please give the correct location")
    }
  }
  answer
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
       gama_path <- setup_gama_ui()
       defpath(gama_path)
  }
}
