# Defines the GAMA repository --------------------------------------------------

gama_repo <- function(repo = NULL) {
  if (! is.null(repo)) options(rama.repo = repo)
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
