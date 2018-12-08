#  mercredi 12 avril 2017
#' Define the GAMA path
#'
#' Define the path of GAMA Java plugins.
#'
#' @param path Absolute path to the GAMA folder.
#' @keywords internal
#defpath <- function(path) {
#  gamapath <- paste0(path,"/plugins")
#  plugins <- grep("*.jar",dir(gamapath),value=T)
#  options(gamar.plugins=paste(paste0(gamapath,"/",plugins),collapse=":"))
#}

onload <- function(){
  options(rama.workspace=paste0(getwd(),"/workspace"))
  options(rama.temp_dir=paste0(options("rama.workspace"),"/temp"))
  options(rama.gama_dir=paste0(options("rama.workspace"),"/gama"))

  options(rama.default.gama.win = "/latest_gama_win.zip");
  options(rama.default.gama.win.appdir = "");
  options(rama.default.gama.osx = "/Gama1.7-macosx.cocoa.x86_64.zip");
  options(rama.default.gama.osx.appdir = "Gama.app");
  options(rama.default.gama.linux = "latest_gama_linux.zip");
  options(rama.default.gama.linux.appdir = "");
  options(rama.repo="file:///Users/nicolas/git/gama/gama/ummisco.gama.product/target/products")
}

gama_repo <- function(repo=NULL){
  if(! is.null(repo)) {
    options(rama.repo=repo)
  }
}

gama_remote_distrib <- function() {
  os <- paste0(Sys.info()["sysname"])
  switch(os,
         "Darwin" = paste0(options("rama.repo"),options("rama.default.gama.osx")),
         "Window" = "truc",
         "linux" = "bidule");
}


download_gama <- function(){
  distrib <- gama_remote_distrib();
  path <- paste0(options("rama.temp_dir"),"/");
  distrib_file <- paste0(path,"downloaded_gama.zip");
  export_dir <-  paste0(options("rama.gama_dir"), "/");
  if(!dir.exists(path))
    dir.create(path, recursive = TRUE);
  download.file(distrib, distrib_file, quiet = FALSE, mode = "wb",cacheOK = TRUE);
  unzip(distrib_file, exdir = export_dir );
  file.remove(distrib_file);

  os <- paste0(Sys.info()["sysname"])
  gama_app <- switch(os,
         "Darwin" = options("rama.default.gama.osx.appdir"),
         "Window" = options("rama.default.gama.win.appdir"),
         "linux" = options("rama.default.gama.linux.appdir")
         );
  paste0(export_dir,gama_app);
}

download_gama_ui <- function(){
  repeat {
    answer <- toupper(readline(prompt = "Do you want to download the last version of Gama? [y/n] "));
    if (answer[1] == "Y" | answer[1] == "N") {
      break;
    }
    else
    {
      print("Sorry, I din't understand... try again");
    }
  }

  if(answer[1]=="Y")
  {
    gama_path <- download_gama();
    defpath(gama_path);
  }
}

defpath <- function(path) {
  os <- paste0(Sys.info()["sysname"])
  # if we use OSX, the plugin is located in the Contents/eclipse sub directory of
  # gama otherwise it is at its root:
  subpath <- ifelse(os=="Darwin","/Contents/eclipse","")
  gamapath <- paste0(path,subpath,"/plugins")
  plugins <- grep("org.eclipse.equinox.launcher_.*",dir(gamapath),value=T)
  options(gamar.plugins = paste(paste0(gamapath,"/",plugins),collapse=":"))
  defaultjar <- paste0(gamapath,"/",plugins)
  options(gamar.startjar=defaultjar)
  options(gamar.Xmx="2048m")
  options(gamar.Xms="512m")
}

################################################################################

.onAttach <- function(...) {
  onload();
  packageStartupMessage("Welcome to gamar v1.0!")
  packageStartupMessage("WARNING: GAMA 1.7.0 needs to be installed on your machine.")
  packageStartupMessage("Check www.gama-platform.org for installation instructions.")
#  defpath("/Users/choisy/Applications/Gama")
#  defpath("/Applications/Gama.app")


}

################################################################################

.onDetach <- function(...) {

#  options(gamar.plugins=NULL)
  options(gamar.startjar=NULL)
  options(gamar.Xmx=NULL)
  options(gamar.Xms=NULL)
}
