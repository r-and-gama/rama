# set_model---------------------------------------------------------------------
#' This function allows to change the model path of an experiment object
#'
#' @param model Path of new gaml model file
#' @param exp Experiment in question
#'
#' @examples
#' exp1 <- load_experiment("sir", system.file("examples", "sir.gaml",
#'                         package = "rama"))
#' model(exp1)
#' set_model(exp1, system.file("examples", "CopyOfsir.gaml",
#'                         package = "rama"))
#' model(exp1)
#'
#' @export

set_model <- function(model, exp){
  # check if experiment name and type are valid in the requested model
  check_experiment(expname(exp), model)
  # check if model parameters and observed parameters correspond
  invisible(capture.output(tmp <- load_experiment(expname(exp),
                                                  model,
                                                  dir = tempfile(c("abcd")))))

  if(all(parameters(exp) == parameters(tmp)) &
     all(obs_rates(exp) == obs_rates(tmp)) &
     any(names(exp) == "tmax") &
     any(names(exp) == "seed"))
    attr(exp, "model") <- model
  else
    stop(paste0("Either Parameters or observation rates or tmax or seed in \"",
              exp, "\" doesn't match with the requested model \"", model, "\""))
  return(exp)
}
