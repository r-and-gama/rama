###################
# change experiment object into a readable data.frame with additionnal attributes
expe2df<-function(expe) {
  df<-data.frame(matrix(unlist(expe[[1]][[1]]),ncol=4,byrow=TRUE))
  names(df)<-c("name","type","value","var")
  df$value<-as.numeric(as.character(df$value))
  attributes(df)$output<-expe[[1]][[2]]$Output
  attributes(df)$others<-expe[[1]][[3]]
  return(df)
}
###################

###################
# read the names of the XML outputs
readNamesXMLOutputs<-function(xml) {
  suppressWarnings(myXML<-readLines(xml))
  idx<-grep("<Variable name=",myXML)
  myXML<-myXML[idx]
  end<-regexpr("'>",myXML)
  begin<-regexpr("name=",myXML)
  unique(substr(myXML,begin+6,end-1))
}
