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
  defpath("/Applications/Gama.app")


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
