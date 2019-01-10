# save_to_gama -----------------------------------------------------------------
#' Save an experiment plan to a GAMA XML file
#'
#' Save an object of class \code{experiment} to an XML file GAMA-compliant.
#'
#' @param exp An object of class \code{experiment}.
#' @param file The path to the output XML file.
#'
#' @importFrom XML xmlToList xmlParse xmlOutputDOM saveXML
#' @examples
#' #load experiment
#' gaml_file <- system.file("examples", "sir.gaml", package = "rama")
#' exp1 <- load_experiment("sir", gaml_file, "sir")
#'
#' save_to_gama(exp1)
#'
#' @export
save_to_gama <- function(exp, file) UseMethod("save_to_gama")

#' @rdname save_to_gama
#' @export
save_to_gama.experiment <- function(exp, file = "out.xml") {
  xmlFile <- xmlOutputDOM(tag = "Experiment_plan")
  id_simulation <- 0
  for (row_id in 1:nrow(exp)) {
    attrib <- c(id         = row_id,
                seed       = exp[row_id, ]$seed,
                finalStep  = exp[row_id, ]$tmax,
                sourcePath = model(exp),
                experiment = name(exp))
    xmlFile$addTag("Simulation", attrs = attrib, close = FALSE)
    xmlFile$addTag("Parameters", close = FALSE)
    y <- parameters(exp[row_id, ])
    for (col_id in 1:ncol(y)) {
      param <- y[, col_id, drop = FALSE]
      title <- substr(names(param), 3, nchar(names(param)))
      title <- as.vector(attr(exp, "dic_rev")[title])
      val <- param[1, 1]
      m_type <- "STRING"
      if (is.numeric(val)) {
        if (is.integer(val)) {
          m_type <- "INT"
        } else m_type <- "FLOAT"
      }
      attribut <- c(name = title,
                    type = m_type,
                    value = val)
      xmlFile$addTag("Parameter", attrs = attribut)
    }
    xmlFile$closeTag()
    xmlFile$addTag("Outputs", close = FALSE)
    y <- obs_rates(exp[row_id, ])
    id_out <- 0
    for (col_id in 1:ncol(y)) {
      param <- y[, col_id, drop = FALSE]
      title <- substr(names(param), 3, nchar(names(param)))
      title <- as.vector(attr(exp, "dic_rev")[title])
      val <- param[1, 1]
      attribut <- c(id        = id_out,
                    name = title,
                    framerate = val)
      id_out <- id_out + 1
      xmlFile$addTag("Output", attrs = attribut)
    }
    xmlFile$closeTag()
    xmlFile$closeTag()
  }
  xmlFile$closeTag()
  saveXML(xmlFile$value(), file)
  normalizePath(file)
}
