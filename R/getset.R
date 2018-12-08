# mercredi 12 avril 2017
getsimulationparam <- function(attrnam,dictionary)
  dictionary$Simulation$.attrs[attrnam]

################################################################################

setsimulationparam <- function(attrnam,dictionary,value) {
  dictionary$Simulation$.attrs[attrnam] <- value
  dictionary
}

################################################################################

getsimulationid <- function(dictionary)
  getsimulationparam("id",dictionary)

################################################################################

setsimulationid <- function(dictionary,value)
  setsimulationparam("id",dictionary,value)

################################################################################

getexperimentname <- function(dictionary)
  getsimulationparam("experiment",dictionary)

################################################################################

setexperimentname <- function(dictionary,value)
  setsimulationparam("experiment",dictionary,value)

################################################################################

getseed <- function(dictionary)
  getsimulationparam("seed",dictionary)

################################################################################

setseed <- function(dictionary,value)
  setsimulationparam("seed",dictionary,value)

################################################################################

getmodelpath <- function(dictionary)
  getsimulationparam("sourcePath",dictionary)

################################################################################

setmodelpath <- function(dictionary,value)
  setsimulationparam("sourcePath",dictionary,value)

################################################################################

getfinalstep <- function(dictionary)
  getsimulationparam("finalStep",dictionary)

################################################################################

setfinalstep <- function(dictionary,value)
  setsimulationparam("finalStep",dictionary,value)

################################################################################

getoutputnames <- function(dictionary) {
  simoutput <- dictionary$Simulation$Outputs
  i <- 1
  outlist <- list()
  while(i<length(simoutput)+1) {
    out <- simoutput[i]$Output["name"]
    outlist <- append(outlist,out)
    i <- i + 1
  }
  as.character(outlist)
}

################################################################################

getparameternames<- function(dictionary) {
  siminput <- dictionary$Simulation$Parameters
  i <- 1
  outlist <- list()
  while(i < length(siminput)+1) {
    out <- siminput[i]$Parameter["name"]
    outlist <- append(outlist,out)
    i <- i + 1
  }
  as.character(outlist)
}

################################################################################

setparametervalue <- function(dictionary,name,value) {
  siminput <- dictionary$Simulation$Parameters
  i <- 1
  while(i<length(siminput)+1) {
    if(name == dictionary$Simulation$Parameters[i]$Parameter["name"])
      dictionary$Simulation$Parameters[i]$Parameter["value"] <- value
    i <- i + 1
  }
  dictionary
}

################################################################################

setoutputframerate <- function(dictionary,name,value) {
  siminput <- dictionary$Simulation$Outputs
  outlist <- list()
  i <- 1
  while(i<length(siminput)+1) {
    if(name == dictionary$Simulation$Outputs[i]$Output["name"] )
    {
      dictionary$Simulation$Outputs[i]$Output["framerate"] <- value
    }
    i <- i + 1
  }
  dictionary
}

################################################################################

getdefaultexperimentplanname <- function(experimentplan)
  experimentplan$Simulation$.attrs["experiment"]

################################################################################

#' Get parameters of a gama model
#'
#' Load experiment meta-data from a model file and an experimentname
#'
#' @inheritParams run
#' @param modelfile relative or absolute path pointing on your model file
#' @param experimentname name of the loaded experiment
#' @keywords internal
getmodelparameter <- function(modelfile,experimentname) {
  cat(paste0("Loading experiment '",experimentname,"' from file '",basename(modelfile),"'...\n"))
  outfile <- createmodelparameterfilename(experimentname)
  outputDisplay <- ""
  if(iswindows()==FALSE)
  {
    outputDisplay <-">/dev/null"
  }
  trycommand <- system(paste0("java -jar \"",getOption("gamar.startjar"),"\" -Xms",
                              getOption("gamar.Xms")," -Xmx",
                              getOption("gamar.Xmx"),
                              " -Djava.awt.headless=true org.eclipse.core.launcher.Main ",
                              "-application msi.gama.headless.id4 -xml ",
                              experimentname," \"",modelfile,"\" \"",outfile,"\"",outputDisplay),
                       ignore.stdout=T,ignore.stderr=T)
# removing the "workspace" directory:
  unlink("workspace",T,T)
  if(trycommand>0) return(-1)
  out <- XML::xmlToList(XML::xmlParse(outfile))
  class(out) <- c("experiment",class(out)) # adding the class
  out
}


iswindows <- function()
{
  return (Sys.info()["sysname"]=="Windows")
}


################################################################################

getexperimentid <- function(simulation_result)
  simulation$.attrs["id"]

################################################################################

getoutputfile <- function(path)
  XML::xmlToList(XML::xmlParse(path))

################################################################################

getexperimentoutput <- function(simulation) {
  out2 <- lapply(simulation,function(x)x[which(names(x)=="Variable")])
  unname(sapply(out2[[1]],function(x)x$.attrs))
}

################################################################################

getoutputs <- function(out, outputs)
{
  out2 <- lapply(out,function(x) {x[which(names(x)=="Variable")]})
  thenames <- unname(sapply(out2[[1]],function(x)x$.attrs))
 # sel <- which(thenames[1,] %in% outputs)
  sel <- which(thenames %in% outputs)
  out3 <- lapply(out2,function(x)x[sel])
  out3 <- out3[-length(out3)]
  trc <- c(t(sapply(out3,function(y)sapply(y,function(x)x$text)))) #as.data.frame(t(sapply(out3,function(y)sapply(y,function(x)x$text))),stringsAsFactors=F)
  steps <-c(as.numeric(t(sapply(out[-length(out)],function(x)x$.attrs["id"]))))
  output<- data.frame(steps,trc)
  return(output)
}

