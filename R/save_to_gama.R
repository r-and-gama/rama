# save_to_gama -----------------------------------------------------------------
#' Save an experiment plan to a GAMA XML file
#'
#' Save an object of class \code{experiment} to an XML file GAMA-compliant.
#'
#' @param exp An object of class \code{experiment}.
#' @param parameter_xml_file name of xml parameter file. This file is created
#'                           in the working dirctory of 'exp'. If not specified,
#'                           name of 'exp' is used.
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

  # id_simulation <- 0
  # for (row_id in 1:nrow(exp)) {
  #   attrib <- c(id         = row_id,
  #               seed       = exp[row_id, ]$seed,
  #               finalStep  = exp[row_id, ]$tmax,
  #               sourcePath = model(exp),
  #               experiment = name(exp))
  #   xmlFile$addTag("Simulation", attrs = attrib, close = FALSE)
  #   xmlFile$addTag("Parameters", close = FALSE)
  #   y <- parameters(exp[row_id, ])
  #   for (col_id in 1:ncol(y)) {
  #     param <- y[, col_id, drop = FALSE]
  #     title <- substr(names(param), 3, nchar(names(param)))
  #     title <- as.vector(attr(exp, "dic_rev")[title])
  #     val <- param[1, 1]
  #     m_type <- "STRING"
  #     if (is.numeric(val)) {
  #       if (is.integer(val)) {
  #         m_type <- "INT"
  #       } else m_type <- "FLOAT"
  #     }
  #     attribut <- c(name = title,
  #                   type = m_type,
  #                   value = val)
  #     xmlFile$addTag("Parameter", attrs = attribut)
  #   }
  #   xmlFile$closeTag()
  #   xmlFile$addTag("Outputs", close = FALSE)
  #   y <- obs_rates(exp[row_id, ])
  #   id_out <- 0
  #   for (col_id in 1:ncol(y)) {
  #     param <- y[, col_id, drop = FALSE]
  #     title <- substr(names(param), 3, nchar(names(param)))
  #     title <- as.vector(attr(exp, "dic_rev")[title])
  #     val <- param[1, 1]
  #     attribut <- c(id        = id_out,
  #                   name = title,
  #                   framerate = val)
  #     id_out <- id_out + 1
  #     xmlFile$addTag("Output", attrs = attribut)
  #   }
  #   xmlFile$closeTag()
  #   xmlFile$closeTag()
  # }
  # xmlFile$closeTag()
  saveXML(xmlFile$value(), paste0(output_dir(exp), "/", parameter_xml_file))
  normalizePath(parameter_xml_file)
}

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
# generate xml tags for each parameter
generate_param <- function(param, names){
  purrr::map2(unlist(param), names, function(p, n){
    c(name = n,
      type = get_type(p),
      value = p)
  })
}

# generate xml tags for each observation rate
generate_obsrate <- function(obsrate, names){
  purrr::pmap(list(unlist(obsrate), names, seq_along(unlist(obsrate))),
       function(p, n, i){
    c(id = i - 1,
      name = n,
      framerate = p)
  })
}
