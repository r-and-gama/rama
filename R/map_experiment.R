map_experiment <- function(df, exp) {
  stopifnot(is.data.frame(df))
  stopifnot(is.experiment(exp))

  if (setequal(names(df), names(exp))) {
    attributes(df) <- attributes(exp)
    return(validate(df))
  } else if (all(names(df) %in% names(exp)) & nrow(df) == nrow(exp)) {
      exp[, names(df)] <- df
      return(validate(df))
  }
  stop(cat("A data frame 'df' can be mapped on an experiment 'exp' only\n",
           "if one of the following conditions is met:\n",
           "  - the sets of names of 'df' and 'exp' are identical;",
           "  - the names of 'df' are included in the names of 'exp'",
           "    AND the numbers of rows of 'df' and 'exp' are identical."))
}
