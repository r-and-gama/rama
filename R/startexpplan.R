# mercredi 12 avril 2017

# returns the name of an available subfolder:
# createmodelparameterfilename <- function(experimentname) {
#   outdirectory <- createworkingdirectory()
#   i <- 0
#   repeat {
#     i <- i + 1
#     outfile <- paste0(outdirectory,"/",experimentname,"_",i,".xml")
#     if(!file.exists(outfile)) break
#   }
#   outfile
# }

################################################################################

# output directory name:
#' @export
create_output_dir <- function(experiment_plan, dir = "") {
  wkdir <- get_wkdir(experiment_plan)
  if(dir == ""){
    dir <- gsub("\"","", expname(experiment_plan)) # name of experiment
  }

  i <- 0
  repeat {
    i <- i + 1
    out_dir <- paste0(wkdir, "/", dir, "_", i)
    if(!file.exists(out_dir)) break
  }
  dir.create(out_dir)
  out_dir
}

################################################################################
#' @export

call_gama <- function(experiment_plan, hpc, output_dir, parameter_xml_file) {
  cat(paste0("Running experiment plan ..."))

  output_display <- ""
  if(isWindows()==FALSE)
  {
    output_display <-">/dev/null"
  }

  gama_command <- system(paste0("java -jar \"",getOption("rama.startjar"),"\" -Xms",
                              getOption("rama.Xms")," -Xmx",getOption("rama.Xmx"),
                              " -Djava.awt.headless=true org.eclipse.core.launcher.Main ",
                              "-application msi.gama.headless.id4 -hpc ",hpc," \"",
                              parameter_xml_file,"\" \"",output_dir,"\"", output_display),
                       ignore.stdout=F,ignore.stderr=T)

  if(gama_command>0) return(-1)
  return(dir(path = output_dir, pattern = "[simulation-outputs[:digit:]+]\\.xml",  full.names = TRUE))
}

################################################################################
#' @export
run_experiment <- function(experiment_plan, hpc = 1, output_dir = "", parameter_xml_file = "") {

  if(!is.experiment(experiment_plan)) {
    stop("The argument `experiment_plan` is not an `experiment` object.")
  }

  # make output directory
  if(output_dir=="")
    output_dir <- create_output_dir(experiment_plan, output_dir)

  # generate xml file from experiment_plan
  if(parameter_xml_file == "")
    parameter_xml_file <-  paste0(expname(experiment_plan), ".xml")
  parameter_xml_file <- save_to_gama(experiment_plan, paste0(output_dir, "/", parameter_xml_file))

  # run all the experiments
  outfiles <- call_gama(experiment_plan, hpc, output_dir, parameter_xml_file)

  # get variables names
  vars <- names(experiment_plan)[grep("r_", names(experiment_plan))]
  vars <- substring(vars, 3)
  vars <- as.vector(attr(experiment_plan, "dic")[vars])

  # retrieve all the variables of all the experiments:
  out <- lapply(outfiles, retrieve_results, experiment_plan)

  # deleting the "workspace" folder:
  unlink("workspace", T, T)
  # return output:
  out
}

# get results from output files for all variables (r_)
#' @importFrom XML xmlToDataFrame
retrieve_results <- function(outfile, experiment_plan) {
  # Extract a data frame
  tmp <- XML::xmlToDataFrame(XML::xmlParse(outfile), stringsAsFactors = F)
  # Extract names of the variable
  lst_name <- unlist(XML::xmlToList(XML::xmlParse(outfile)))
  lst_name <- lst_name[which(grepl("name", names(lst_name)))]
  lst_name <- unique(lst_name)

  # Tidy the output
  tmp2 <- lapply(seq_len(dim(tmp)[2]), function(x) {
    suppressWarnings(if (is.numeric(as.numeric(tmp[, x]))) {
      tmp[, x] <- as.numeric(tmp[, x]) } else { tmp[, x] })
  })
  tmp <- as.data.frame(setNames(tmp2, lst_name))

  new_name <- as.vector(attr(experiment_plan, "dic")[lst_name])
  names(tmp) <- new_name
  tmp$Step <- c(0:(dim(tmp)[1] - 1))
  tmp <- tmp[, c("Step", new_name)]
  attr(tmp, "path") <- outfile
  tmp
}

## To be revised
## NOT CALLED
#getoutputs <- function(out, outputs)
#{
#  thenames <- unname(sapply(out2[[1]],function(x)x$.attrs))
#  sel <- which(thenames %in% outputs)
#  out3 <- lapply(out2,function(x) x[sel])
#  out3 <- out3[-length(out3)]
#  trc <- c(t(sapply(out3,function(y)sapply(y,function(x)x$text)))) #as.data.frame(t(sapply(out3,function(y)sapply(y,function(x)x$text))),stringsAsFactors=F)
#  steps <-c(as.numeric(t(sapply(out[-length(out)],function(x)x$.attrs["id"]))))
#  output<- as.data.frame(steps,trc)
#  output
#}

##
################################################################################

cleaning <- function() unlink("workgamar",T,T)
