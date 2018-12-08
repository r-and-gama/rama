#############################################################################



getexperimentid <- function(simulation_result)
{
  return(simulation$.attrs["id"])
}

getexperimentoutput <- function(simulation)
{
  out2 <- lapply(simulation,function(x) {x[which(names(x)=="Variable")]})
  unname(sapply(out2[[1]],function(x)x$.attrs))
}


readxmlfile <- function(xmlfile) {
  # In the "ouput" folder of the local directory they are:
  # A XML file is created by gama headless called "simulation-outputs" suffixed by an Id
  # A list of Snapshots are in the snaphsot
  # parse the xml http://www.informit.com/articles/article.aspx?p=2215520
  # http://stackoverflow.com/questions/31913567/r-xml-to-dataframe-questions Super !
  xmlresult <- xmlParse(xmlfile)
  # transform into a list
  listresult <- xmlToList(xmlresult)
  df <- ldply(listresult, data.frame)
  df <- df[-nrow(df),]
  nbcol <- ncol(df)
  names(df) <- paste(replicate(nbcol,"Col"),seq(1,nbcol),sep="")
  # remove last column
  df <- df[-nbcol]
  # Build the vector of names of the columns that are the values of the odd columns
  # e.g.  Step 993 Susceptible   7 Infected 0 <NA>
  #
  coltodelete <- c(seq(3, nbcol-1, by = 2),1)
  coltoreplace <- seq(2, nbcol-1, by = 2)
  a <- df[1, coltodelete]
  names(df)[coltoreplace] <- sapply(a, as.character)
  #Remove all columns not useful
  df <- df[-coltodelete]
  #The last column contains the step and goes first
  df <- select(df, Step, everything())
  return(df)
}
