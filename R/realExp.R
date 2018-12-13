# realexp --------------------------------------------------------------

#' Restore the correct values of an output
#'
#' It puts a NA whenever an observation was in fact not made in GAMA.
#'
#' From the list of frequencies of each simulation of the experiment
#' the values incorrectly associated to the last value observed are reset to NA.
#'
#' @param exp An object of class \code{experiment}.
#' @param output An object of class \code{list}.
#' @return Returns a list of dataframes, one for each experiment.
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
#'str(otp)
#'newoutput <- realexp(otp,exp0)
#'str(newoutput)
#'
#' @export
realexp <- function(output,exp0){

  newoutput <- list()
  for (j in 1:nrow(exp0))
  {

    curexp <- exp0
    cursimulnum  <- j
    # In the current experiment object, cursimul gives the line of the simulation
    curoutput    <- output[cursimulnum][[1]]
    # build the vector of observed variables
    curobs <- attributes(curoutput)$names[-1]
    # build the index of the variables whose rate is computed
    curobsidx <- sapply(X=curobs, function(x) which(colnames(curoutput) == x))
    # build the vector of text corresponding of the attributes of rates
    observed <- paste("r_",curobs,sep="")
    # build the index of the variables whose rate is computed
    ratesidx <- sapply(X=observed, function(x) which(colnames(curexp) == x))
    # retrieve the value of the rates
    ratesval <- (as.data.frame(curexp)[cursimulnum,ratesidx])

    curoutput    <- output[cursimulnum][[1]]
    for (i in 1:length(as.vector(curobsidx)))
    {
      freq <- as.integer(ratesval[i])
      curvalue <- curoutput[,curobsidx[i]]
      max <- length(curvalue)
      if (freq !=1)
      {
        curvalue[sapply(X=1:max , function(x) (x %% freq !=1))]<- NA
        curoutput[,curobsidx[i]] <- curvalue
      }
    }
    if (length(newoutput) == 0)
    {
      newoutput <- list(curoutput)
    }
    else
    {
      newoutput <- c(newoutput,list(curoutput))
    }
  }
  return(newoutput)
}
