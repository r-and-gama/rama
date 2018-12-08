# mercredi 12 avril 2017
addoutput <- function(dictionary,name,framerate) {
  id <- length(dictionary$Simulation$Outputs)
  Output <- c("framerate"=framerate,"id"=id,"name"=name)
  dictionary$Simulation$Outputs <- append(dictionary$Simulation$Outputs,
                                          list("Output"=Output))
  dictionary
}

################################################################################

removeoutput <- function(dictionary,name)
{
  siminput <- dictionary$Simulation$Outputs
  i <- 0
  while(i<length(siminput)) {
    if(name==dictionary$Simulation$Outputs[i]$Output["name"]) {
      dictionary$Simulation$Outputs <- dictionary$Simulation$Outputs[-i]
      dictionary
    }
    i <- i + 1
  }
}

################################################################################

addtoexperimentplan <- function(simulation,experimentplan=list(), id = length(experimentplan)) {
  simulation <- setsimulationid(simulation,id)
  experimentplan <- append(experimentplan,simulation)
  class(experimentplan) <- "plan"
  class(experimentplan[[length(experimentplan)]]) <- "experiment"
  experimentplan
}
