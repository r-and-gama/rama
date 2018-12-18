# fill_experiment --------------------------------------------------------------
#' Add output of the ran experiment to the experiment object
#'
#' It puts
#'
#'
#' @param output An object of class \code{list}.
#' @param exp An object of class \code{experiment}.
#'
#' @return Returns an experiment that contains its results.
#'
#' @examples
#'df <-   data.frame(S0 = c(900, 800, 500), # this is a data frame of 3 lines
#'                   I0 = c(100, 200, 500),
#'                   R0 = 0,
#'                   beta = 1.5,
#'                   gamma = .15,
#'                   S = c(1,2,3),
#'                   I = c(2,4,6),
#'                   R =c(10,20,30),
#'                   nbiter = 1000,
#'                   seed = "123456789")
#'
#'exp0 <- experiment(
#'  df,
#'  parameters = c("S0","I0","R0","beta","gamma"),
#'  obsrates  = c("S", "I", "R"),
#'  tmax = "nbiter",
#'  seed = "seed",
#'  experiment = "sir",
#'  model = system.file("examples", "sir.gaml", package = "rama"),
#'  dir = "testsir"
#')
#'
#'
#'exp0
#'otp <- run_experiment(exp0)
#'newoutput <- fill_experiment(otp,exp0)
#'
#'
#' @export
fill_experiment <- function(output, exp) {
  if (!(is.list(output) & length(output) == nrow(exp))) {
     for (j in (1:length(output))) {
       # exp[j,"RUN"] <- TRUE
       # exp[j,"RES"] <- output[[j]]
     }
  }
  return(output)
}
