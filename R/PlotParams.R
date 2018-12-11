# plot_parms --------------------------------------------------------------

#' Plot the parameters that have the greatest variance in the experiments of a model.
#'
#' From the list of all parameters of the experiments
#' that do vary (i.e. not null) the one (up to 3) with the biggest variance
#' are ploted in 3D, 2D and 1D.
#' ©JDZ
#' @param x an experiment.
#'
#' @return Returns a vector of the variables with highest variances.
#'
#' @examples
#'
#' plot_parms(exp)
#'
#' @importFrom plot3D scatter3D scatter2D
#'
#' @export
#'

plot_parms <- function(exp) {


  if (nrow(exp) == 0) {
    stop("There is no set of parameters for this experiment in this experiment.")
  }

  allvar <- sapply(exp[,-ncol(exp)], FUN = var)
  allvar <- sort(allvar[ allvar!=0], decreasing = TRUE)
  worthidx <- sapply(X=names(allvar), function(x) which(colnames(exp) == x))
  topidx <- if (length(worthidx) !=0) {worthidx[1:min(3,length(worthidx))]} else {0}
  n <- length(topidx)
  if (n == 3) scatter3D(exp[,topidx[1]], exp[,topidx[2]],exp[,topidx[3]],
                      xlab = colnames(exp)[topidx[1]],
                      ylab = colnames(exp)[topidx[2]],
                      zlab = colnames(exp)[topidx[3]],
                      pch = 18, cex = 2,
                      theta = 20, phi = 20
                      )

  if (n == 2) scatter2D(exp[,topidx[1]], exp[,topidx[2]],
                      xlab = colnames(exp)[topidx[1]],
                      ylab = colnames(exp)[topidx[2]])

  if (n==1) stripchart(exp[,topidx[1]], xlab = colnames(exp)[topidx[1]])
  return(topidx)
}
