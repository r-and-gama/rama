getexpval <- function(object) {
  tmp <- do.call(rbind,object$Parameters)
  parameters <- as.numeric(tmp[,"value"])
  names(parameters) <- tmp[,"name"]
  tmp <- do.call(rbind,object$Outputs)
  framerates <- as.integer(tmp[,"framerate"])
  names(framerates) <- tmp[,"name"]
  list(parameters=parameters,framerates=framerates)
}


print.experiment <- function(object) {
  object <- getexpval(object[[1]])
  cat("\nModel parameters:\n")
  print(object$parameters)
  cat("\nOutputs frame rates:\n")
  print(object$framerates)
  cat("\n")
  invisible(object)
}


print.plan <- function(object) {
  object <- lapply(object,getexpval)
  slotnames <- c("parameters","framerates")
  object <- lapply(slotnames,function(slot)lapply(object,function(x)x[[slot]]))
  object <- lapply(object,function(x)do.call(rbind,x))
  names(object) <- slotnames
  object <- lapply(object,function(x)
    {rownames(x) <- paste0("experiment #",1:nrow(x));x})
  cat("\nModel parameters:\n")
  print(object$parameters)
  cat("\nOutputs frame rates:\n")
  print(object$framerates)
  cat("\n")
  invisible(object)
}
