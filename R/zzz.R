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
