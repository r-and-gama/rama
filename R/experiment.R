saveToGama <- function(x) UseMethod("saveToGama")

saveToGama.experiment <- function(x,file="out.xml")
{
  xmlFile <- xmlOutputDOM( tag="Experiment_plan")
  id_simulation <- 0
  for (row_id in 1:nrow(x)) {
    attrib <- c(id = row_id,
              seed = x[row_id,]$seed,
              finalStep = x[row_id,]$tmax,
              sourcePath = model(x),
              experiment="sir" )
    xmlFile$addTag("Simulation", attrs=attrib,  close=FALSE)
    xmlFile$addTag("Parameters",  close=FALSE)
    y <- parameters(x[row_id,])
    for(col_id in 1:ncol(y))
    {
      param <- y[,col_id, drop = F]
      title <- substr(names(param),3,nchar(names(param)))
      val <- param[1,1]
      m_type <- "STRING"
      if(is.numeric(val))
        if(is.integer(as.numeric(val)))
        {
          m_type <- "INT"
          print(val)
          print("INT")
        }
        else
        {
          m_type <- "FLOAT"
          print(val)
        }
      attribut <- c(name = title,
                    type = m_type,
                    value = val)
      xmlFile$addTag("Parameter", attrs=attribut)
    }
    xmlFile$closeTag()
    xmlFile$addTag("Outputs",  close=FALSE)
    y <- observation(x[row_id,])

    id_out <- 0
    for(col_id in 1:ncol(y))
    {
      param <- y[,col_id, drop = F]
      print(names(param) )
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
  file
}

experiment <- function(x) UseMethod("experiment")

experiment.default <- function() {
  x <- data.frame("seed"=NA,"finalStep"=NA)
  attr(x, "model") <- NA
  attr(x, "parameters") <- c()
  attr(x, "outputs") <- c()
  class(x) <- c("experiment", class(x))
  x
}

experiment.data.frame <- function(x) {
  class(x) <- c("experiment", class(x))
  x
}

get_parameters <- function(x) UseMethod("get_parameters")
add_parameters <- function(x, colName) UseMethod("add_parameters")
get_outputs <- function(x) UseMethod("get_outputs")
add_outputs <- function(x) UseMethod("add_outputs")
get_seeds <- function(x) UseMethod("get_seeds")
set_seeds <- function(x) UseMethod("set_seeds")
get_final_steps <- function(x) UseMethod("get_final_steps")
set_final_steps <- function(x) UseMethod("set_final_steps")
get_model <- function(x, name) UseMethod("get_models")
set_model <- function(x) UseMethod("set_models")

get_parameters.experiment <- function(x)
{
  attr(x, "parameters")
}

get_outputs.experiment <- function(x)
{
  attr(x, "outputs")
}

get_final_steps.experiment <- function(x)
{
  x$finalStep
}

set_final_steps.experiment <- function(x, finalStep = NULL)
{
  if(is.null(finalStep))
    finalStep <- rep(1, nrow(x))
  else if( inherits(x,"numeric"))
    finalStep <- rep(finalStep, nrow(x))
  x$finalStep <- finalStep
  x
}

get_seeds.experiment <- function(x)
{
  x$seed
}

set_seeds.experiment <- function(x, seed = NULL)
{
  if(is.null(seed))
    seed <- rep(1, nrow(x))
  else if( inherits(x,"numeric"))
    seed <- rep(seed, nrow(x))
  x$seed <- seed
  x
}

add_parameters.experiment <- function(x,colName = "newCol") {
  x[ , colName] <- rep(NA,nrow(x))
  attr(x, "parameters") <- append(attr(x, "parameters"),colName)
  x
}

add_outputs.experiment <- function(x,colName = "newCol") {
  x[ , colName] <- rep(NA,nrow(x))
  attr(x, "outputs") <- append(attr(x, "outputs"),colName)
  x
}

get_model <- function(x)
{
  attr(x, "model")
}

set_model <- function(x, m_name)
{
  attr(x, "model") <- m_name
  x
}


