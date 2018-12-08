# mercredi 12 avril 2017
# working directory:
createworkingdirectory <- function() {
  outdirectory <- paste0(getwd(),"/workgamar")
  if(!file.exists(outdirectory)) dir.create(outdirectory)
  outdirectory
}

################################################################################

# returns the name of an available subfolder:
createmodelparameterfilename <- function(experimentname) {
  outdirectory <- createworkingdirectory()
  i <- 0
  repeat {
    i <- i + 1
    outfile <- paste0(outdirectory,"/",experimentname,"_",i,".xml")
    if(!file.exists(outfile)) break
  }
  outfile
}

################################################################################

# output directory name:
createoutputdirectoryname <- function(experimentplan) {
  outdirectory <- createworkingdirectory()
  defaultname <- getdefaultexperimentplanname(experimentplan)
  i <- 0
  repeat {
    i <- i + 1
    outfile <- paste0(outdirectory,"/out_",defaultname,"_",i)
    if(!file.exists(outfile)) break
  }
  outfile
}

################################################################################

#' store experimentplan
#'
#' Save an experiment plan to an xml file. it returns the path of the xml file
#'
#' @inheritParams run
#' @param experimentplan  experiment plan to store



#  writemodelparameterfile <- function(experimentplan) {
#    outfile <- createmodelparameterfilename(getdefaultexperimentplanname(experimentplan))
#    xml <- buildxmlfromexperimentplan(experimentplan)
#    write(xml,outfile,sep="")
#    outfile
#  }

writemodelparameterfile <- function(experimentplan,outfile ="") {
  if(outfile == "")
    outfile <- createmodelparameterfilename(getdefaultexperimentplanname(experimentplan))
   xml <- buildxmlfromexperimentplan(experimentplan)
  write(xml,outfile,sep="")
  outfile
}


################################################################################

startexperimentplan <- function(experimentplan,hpc=1,outputdirectory="") {
  cat(paste0("Running experiment plan ..."))
  parameterxmlfile <- writemodelparameterfile(experimentplan)
  if(outputdirectory=="")
    outputdirectory <- createoutputdirectoryname(experimentplan)

  outputDisplay <- ""
  if(iswindows()==FALSE)
  {
    outputDisplay <-">/dev/null"
  }

  trycommand <- system(paste0("java -jar \"",getOption("gamar.startjar"),"\" -Xms",
                              getOption("gamar.Xms")," -Xmx",getOption("gamar.Xmx"),
                              " -Djava.awt.headless=true org.eclipse.core.launcher.Main ",
                              "-application msi.gama.headless.id4 -hpc ",hpc," \"",
                              parameterxmlfile,"\" \"",outputdirectory,"\"", outputDisplay),
                       ignore.stdout=F,ignore.stderr=T)

  if(trycommand>0) return(-1)
  return(dir(path = outputdirectory, pattern = "*.xml",  full.names = TRUE))
}

################################################################################

runexpplan <- function(plan,hpc = 1) {
  # run all the experiments of the plan:
  outfiles <- startexperimentplan(plan,hpc)
  # retrieve the variables names of each experiment:
  vars <- lapply(plan,function(x)getoutputnames(list(Simulation=x)))
  # fct1 retrieves variable "var" of experiment "exp"
  fct1 <- function(exp,var)
    {
    res1 <-getoutputfile(exp)
    res <- getoutputs(res1,var)
    res
    }
  # fct2 calls fct1 to retrieve variables "vars" of experiment "exp". If the time
  # frames of all the variables are the same, it further aggregates them in a
  # data frame.
  fct2 <- function(exp,vars) {
    tmp <- lapply(vars,function(x)fct1(exp,x))
    if(length(unique(sapply(tmp,length)))<2) {
      suppressWarnings(tmp <- Reduce(function(...)merge(...,by="steps"),tmp))
      names(tmp) <- c("step",vars)
    }
    attr(tmp,"path") <- exp
    tmp
  }
  # retrieving all the variables of all the experiments:
  out <- mapply(fct2,outfiles,vars,SIMPLIFY=F)
  # deleting the "workspace" folder:
  unlink("workspace",T,T)
  # return output:
  out
}
##
################################################################################

cleaning <- function() unlink("workgamar",T,T)
