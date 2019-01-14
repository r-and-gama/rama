# infer type of simulation values
get_type <- function(val){
  case_when(
    is.character(val) ~ "STRING",
    is.integer(val) ~ "INT",
    is.double(val) ~ "FLOAT",
    is.factor(val) ~ "STRING",
    is.na(val) ~ "STRING",
    is.null(val) ~ "STRING",
    TRUE ~ "STRING"
  )

}

# ------------------------------------------------------------------------------
# generate xml tags for each parameter
generate_param <- function(param, names){
  purrr::map2(unlist(param), names, function(p, n){
    c(name = n,
      type = get_type(p),
      value = p)
  })
}

# ------------------------------------------------------------------------------
# generate xml tags for each observation rate
generate_obsrate <- function(obsrate, names){
  purrr::pmap(list(unlist(obsrate), names, seq_along(unlist(obsrate))),
              function(p, n, i){
                c(id = i - 1,
                  name = n,
                  framerate = p)
              })
}

# save_to_gama -----------------------------------------------------------------
#' Save an experiment plan to a GAMA XML file
#'
#' Save an object of class \code{experiment} to an XML file GAMA-compliant.
#'
#' @param exp An object of class \code{experiment}.
#' @param parameter_xml_file name of XML parameter file. This file is created
#'                           in the working directory of `exp`. If not
#'                           specified, name of `exp`` is used.
#'
#' @importFrom XML xmlToList xmlParse xmlOutputDOM saveXML
#' @importFrom purrr map2 pmap
#' @importFrom dplyr case_when
#' @examples
#' #load experiment
#' gaml_file <- system.file("examples", "sir.gaml", package = "rama")
#' exp1 <- load_experiment("sir", gaml_file, "sir")
#'
#' save_to_gama(exp1)
#'
#' @rdname save_to_gama
#' @export

save_to_gama <- function(exp, parameter_xml_file) UseMethod("save_to_gama")

#' @rdname save_to_gama
#' @export
save_to_gama.default <- function(exp, parameter_xml_file)
                        "Unknown class"

#' @rdname save_to_gama
#' @export
save_to_gama.experiment <- function(exp, parameter_xml_file = "") {

  params <- parameters(exp)
  param_names <- attr(exp, "dic_rev")[gsub("p_", "", names(params))]
  params <- as.list(as.data.frame(t(params)))

  obsrates <- obs_rates(exp)
  obsrates_names <- attr(exp, "dic_rev")[gsub("r_", "", names(obsrates))]
  obsrates <- as.list(as.data.frame(t(obsrates)))

  simulations <- as.list(as.data.frame(rbind(id = row.names(exp),
                       seed = exp$seed,
                       finalStep = exp$tmax,
                       sourcePath = model(exp),
                       experiment = name(exp)),
                       stringsAsFactors = FALSE))
  names(simulations) <- row.names(exp)
  exp_lst <- list(simul = simulations,
                  param = params,
                  obsrate = obsrates)

  xmlFile <- xmlOutputDOM(tag = "Experiment_plan")
  pmap(exp_lst, function(simul, param, obsrate){

    names(simul) <- c("id", "seed", "finalStep", "sourcePath", "experiment")
    xmlFile$addTag("Simulation", attrs = simul, close = FALSE)

    param_lst <- generate_param(param, param_names)
    xmlFile$addTag("Parameters", close = FALSE)
    lapply(param_lst, function(x) xmlFile$addTag("Parameter", attrs = x))
    xmlFile$closeTag()

    obsrate_lst <- generate_obsrate(obsrate, obsrates_names)
    xmlFile$addTag("Outputs", close = FALSE)
    lapply(obsrate_lst, function(x) xmlFile$addTag("Output", attrs = x))
    xmlFile$closeTag()

    xmlFile$closeTag()
  })

  if (parameter_xml_file == "")
    parameter_xml_file <-  paste0(name(exp), ".xml")
  parameter_xml_file <- paste0(output_dir(exp), "/", parameter_xml_file)
  saveXML(xmlFile$value(), file = parameter_xml_file)
  normalizePath(parameter_xml_file)
}
