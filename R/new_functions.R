# get_parameters ---------------------------------------------------------------


get_parameters <- function(x) {
  x2 <- do.call(rbind, x[["Parameters"]])
  x3 <- do.call(data.frame, as.list(as.numeric(x2[, "value"])))
  x3 <- setNames(x3, x2[, "name"])
  sel <- grep("INT", x2[, "type"])
  x3[, sel] <- lapply(x3[, sel], as.integer)
  x3
}



get_variables <- function(x) {
  x2 <- do.call(rbind, x[["Outputs"]])
  x3 <- do.call(data.frame, as.list(as.numeric(x2[, "framerate"])))
  x3 <- setNames(x3, x2[, "name"])
  x3[] <- lapply(x3, as.integer)
  x3
}



#' @importFrom stats setNames
get_attributes <- function(x) {
  out <- setNames(do.call(function(...) data.frame(..., stringsAsFactors = FALSE),
                          as.list(x$.attrs[c("finalStep", "seed", "sourcePath", "experiment")])),
                  c("tmax", "seed", "gaml", "experiment"))
  out$tmax <- as.integer(out$tmax)
  out$seed <- as.numeric(out$seed)
  out
}

# load_experiment --------------------------------------------------------------

#' Load An Experiment
#'
#' Loads an experiment from a model specified in a \code{gaml} file and returns
#' an object of class \code{plan}.
#'
#' @param experiment The name of the experiment to load.
#' @param model The name of the file from which to load the experiment.
#'
#' @importFrom XML xmlToList xmlParse
#'
#' @export
load_experiment <- function(experiment, model) {
  message("Loading experiment '", experiment,
          "' from file '", basename(model), "'...")
  tmp <- tempfile(fileext = ".xml")
  system(paste0("java -jar ", getOption("rama.startjar"),
                " -Xms", getOption("rama.Xms"),
                " -Xmx", getOption("rama.Xmx"),
                " -Djava.awt.headless=true org.eclipse.core.launcher.Main",
                " -application msi.gama.headless.id4 -xml ",
                experiment, " ", model, " ", tmp, " > /dev/null"),
         ignore.stdout = TRUE, ignore.stderr = TRUE)
  unlink("workspace", TRUE, TRUE) # removes the above-created workspace directory
  out <- xmlToList(xmlParse(tmp))$Simulation
  out <- lapply(c(get_parameters, get_variables, get_attributes), function(f) f(out))
  names(out[[1]]) <- paste0("p_", names(out[[1]]))
  names(out[[2]]) <- paste0("r_", names(out[[2]]))
  out <- do.call(cbind, out)
  class(out) <- c("experiment", class(out))
  attr(out, "model") <- as.character(unname(out$gaml))
  attr(out, "experiment") <- as.character(unname(out$experiment))
  out$gaml <- NULL
  out$experiment <- NULL
  out
}

#' @export
save_to_gama <- function(plan,file) UseMethod("save_to_gama")

# save_to_gama --------------------------------------------------------------

#' Save an experiment plan to gama xml file
#'
#' Save an xml \code{file} containing the experiment plan defined in the object of class \code{experiment}.
#'
#' @param plan Object of class experiment containing all experiment to do
#' @param file The path where you want to save the file.
#'
#' @importFrom XML xmlToList xmlParse
#'
#' @export
save_to_gama.experiment <- function(plan,file="out.xml")
{
  xmlFile <- xmlOutputDOM( tag="Experiment_plan")
  id_simulation <- 0
  for (row_id in 1:nrow(plan)) {
    attrib <- c(id = row_id,
                seed = plan[row_id,]$seed,
                finalStep = plan[row_id,]$tmax,
                sourcePath = model(plan),
                experiment=expname(plan))
    xmlFile$addTag("Simulation", attrs=attrib,  close=FALSE)
    xmlFile$addTag("Parameters",  close=FALSE)
    y <- parameters(plan[row_id,])
    for(col_id in 1:ncol(y))
    {
      param <- y[,col_id, drop = F]
      title <- substr(names(param),3,nchar(names(param)))
      val <- param[1,1]
      m_type <- "STRING"
      if(is.numeric(val))
        if(is.integer(val))
        {
          m_type <- "INT"
        }
      else
      {
        m_type <- "FLOAT"
      }
      attribut <- c(name = title,
                    type = m_type,
                    value = val)
      xmlFile$addTag("Parameter", attrs=attribut)
    }
    xmlFile$closeTag()
    xmlFile$addTag("Outputs",  close=FALSE)
    y <- observation(plan[row_id,])
    id_out <- 0
    for(col_id in 1:ncol(y))
    {
      param <- y[,col_id, drop = F]
      title <- substr(names(param),3,nchar(names(param)))
      val <- param[1,1]
      attribut <- c(id = id_out,
                    name = title,
                    framerate = val)
      id_out <- id_out + 1
      xmlFile$addTag("Output", attrs=attribut)
    }
    xmlFile$closeTag()
    xmlFile$closeTag()
  }
  xmlFile$closeTag()
  saveXML(xmlFile$value(), file)
  normalizePath(file)
}







#' @export
model <- function(x) UseMethod("model")

#' @export
model.default <- function(x) "Unknown class"

#' @export
model.experiment <- function(x) {
  attributes(x)$model
}


#' @export
expname <- function(x) UseMethod("expname")

#' @export
expname.default <- function(x) "Unknown class"

#' @export
expname.experiment <- function(x) {
  attributes(x)$experiment
}



#' @export
parameters <- function(x) UseMethod("parameters")

#' @export
parameters.default <- function(x) "Unknown class"

#' @export
parameters.experiment <- function(x) {
  x[, grep("^p_", names(x), value = TRUE)]
}


#' @export
observation <- function(x) UseMethod("observation")

#' @export
observation.default <- function(x) "Unknown class"

#' @export
observation.experiment <- function(x) {
  x[, grep("^r_", names(x), value = TRUE)]
}


#' @export
repl <- function(x, n) UseMethod("repl")

#' @export
repl.default <- function(x, n) "Unknown class"

#' @export
repl.experiment <- function(x, n) {
  do.call(rbind, lapply(1:n, function(y) x))
}

# ------------------------------------------------------------------------------

#' @export
#'
#' @examples
#' my_experiment <- experiment(
#'   expand.grid(S0 = c(900, 950, 999),
#'               I0 = c(100, 50, 1),
#'               R0 = 0,
#'               beta = 1.5,
#'               gamma = .15),
#'   data.frame(S = 1, I = 1, R = 1),
#'   tmax = 1000,
#'   seed = 1,
#'   model = "/Users/choisy/Dropbox/aaa/r-and-gama/rama/inst/examples/sir.gaml"
#' )
#' # If we want to change the seeds:
#' my_experiment$seed <- 1:9
experiment <- function(parameters, obsrates, tmax, seed, model, experiment) {
  names(parameters) <- paste0("p_", names(parameters))
  names(obsrates) <- paste0("r_", names(obsrates))
  structure(data.frame(parameters, obsrates,
                       tmax = tmax,
                       seed = seed),
            model = model,
            experiment = experiment,
            class = c("experiment", "data.frame"))
}
