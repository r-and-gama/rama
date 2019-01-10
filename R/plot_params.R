# plot_params --------------------------------------------------------------

#' Visualize values of parameters
#'
#' Plot the parameters that have the greatest variance in the experiments of a model.
#'
#' From the list of all parameters of the experiments that do vary (i.e. not
#' null) the one (up to 3) with the biggest variance are ploted in 3D, 2D and 1D.
#'
#' @param exp An object of class \code{experiment}.
#'
#' @importFrom stats var
#' @importFrom graphics stripchart
#'
#' @return Returns a vector of the variables with highest variances.
#'
#' @examples
#'df <-   data.frame(S0 = c(900, 800, 500), # this is a data frame of 3 lines
#'                   I0 = c(100, 200, 500),
#'                   R0 = 0,
#'                   beta = c(1.4,1.5,1.6),
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
#' plot_params(exp0)
#'
#' @importFrom plot3D scatter3D scatter2D
#'
#' @export
#'
plot_params <- function(exp) {
  if (nrow(exp) == 0) return("There is no simulation in this experiment")
  if (nrow(exp) == 1) return(
    "There is only one simulation in this experiment so no ")

  allvar <- sapply(parameters(exp), var)
  allvar <- sort(allvar[ allvar != 0], decreasing = TRUE)
  if (length(allvar) == 0) return(paste(
    "There is only one set of parameters for these", nrow(exp), "experiments"))

  worthidx <- sapply(X = names(allvar), function(x) which(colnames(exp) == x))
  topidx <- if (length(worthidx) != 0) {
    worthidx[1:min(3, length(worthidx))]
    } else {
      0
    }
  n <- length(topidx)

  # check n the number of parameters to be plotted
  # if n is equal to 0
  if (n == 0) stop(paste0("There is no parameters to plot in this experiment"))
  if (n == 1) stripchart(exp[, topidx[1]], xlab = colnames(exp)[topidx[1]])
  if (n == 2) scatter2D(exp[, topidx[1]], exp[, topidx[2]],
                      xlab = colnames(exp)[topidx[1]],
                      ylab = colnames(exp)[topidx[2]])
  if (n == 3) scatter3D(exp[, topidx[1]], exp[, topidx[2]], exp[, topidx[3]],
                        xlab = colnames(exp)[topidx[1]],
                        ylab = colnames(exp)[topidx[2]],
                        zlab = colnames(exp)[topidx[3]],
                        pch = 18, cex = 2,
                        theta = 20, phi = 20
  )
  # returns the parameters of the ploted parameter(s)
  return(topidx)
}
