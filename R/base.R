# indexes_first_and_last -------------------------------------------------------
#' @param x A vector of characters.
#' @param n The number of elements to extract from the vector x. Should be > 1.
#'
#' @return A subvector of n elements of x
#'
#' @details If n = 2, it returns the first and the last elements, if n = 3, it
#'          returns the first 2 and the last elements, if n = 4, it returns the
#'          first 2 and the last 2 elements, if n = 5, it returns the first 3
#'          and the last 2 elements, and so on...
#'
#' @noRd
indexes_first_and_last <- function(x, n) {
  l <- length(x)
  x[c(1:rep(1:l, each = 2)[n], rep(l:1, each = 2)[n - 1]:l)]
}

# get_width --------------------------------------------------------------------
#' @param x A vector of characters.
#' @param n The targeted width, in number of characters, we would like.
#'
#' @return The actual width, in number of characters, we get.
#'
#' @noRd
get_width <- function(x, n) {
  x <- nchar(indexes_first_and_last(x, n))
  sum(x) + length(x) - 1
}

# names_of_left_and_right ------------------------------------------------------

#' @param x A vector of characters.
#' @param th The targeted width, in number of characters, we would like.
#'
#' @return A list of two vectors of characters. The first element corresponds to
#'         the left part and the second element corresponds to the right part.
#'
#' @noRd
names_of_left_and_right <- function(x, th) {
  tmp <- sapply(2:length(x), get_width, x = x) > th
  if (all(tmp)) tmp <- x[c(1, length(x))]
  else {
    if (all(! tmp)) tmp <- x
    else tmp <- indexes_first_and_last(x, which(tmp)[1])
  }
  sel <- 1:round(length(tmp) / 2)
  list(tmp[sel], tmp[-sel])
}

# insert_middle ----------------------------------------------------------------

#' @param x A data frame.
#' @param n The width, in number of characters we wish the data frame.
#'
#' @return A data frame with reduced number of columns.
#'
#' @examples
#' insert_middle(as.data.frame(exp5), 20)
#'
#' @noRd
insert_middle <- function(x, n, digits = 4) {
  x <- round(x, digits)
  a <- names_of_left_and_right(names(x), n)
  if (sum(sapply(a, length)) < length(x)) {
    left <- x[, a[[1]], drop = FALSE]
    right <- x[, a[[2]], drop = FALSE]
    middle <- setNames(data.frame(".", ".", ".",
                                  stringsAsFactors = FALSE), rep(".", 3))
    return(cbind(left, middle, right))
  }
  x
}

# print.experiment method ------------------------------------------------------
#' @importFrom utils head tail
#' @export
print.experiment <- function(x, interspace = 3, n = 6, digits = 4,
                             nchar = 50, ...) {

  attrs <- attributes(x)

  print_info <- function() {
    cat(  "experiment name:    ", attrs$experiment,
          "\ninput gaml file:    ", attrs$model,
          "\noutput directory:   ", attrs$wkdir, "\n")
  }

  if (ncol(x) < 1) {

    cat(
      "Experiment without any simulation, ",
      "tunable parameter or observed variable\n")
    print_info()

  } else {

    s <- function(x) ifelse(x > 1, "s", "")
    param <- parameters(x)
    obser <- obs_rates(x)
    nsim <- nrow(x)
    npar <- ncol(param)
    nvar <- ncol(obser)

    cat("Experiment with ", nsim, " simulation"       , s(nsim),
        " of "            , npar, " parameter"        , s(npar),
        " and "           , nvar, " observed variable", s(nvar), "\n", sep = "")
    print_info()
    cat("model parameters:   ", paste(names(param), collapse = ", "),
        "\nobserved variables: ", paste(names(obser), collapse = ", "),
        "\nExperiment overview:\n")

    if (ncol(param) > 2) param2 <- insert_middle(param, nchar, digits)
    else param2 <- param
    if (ncol(obser) > 2) obser2 <- insert_middle(obser, nchar, digits)
    else obser2 <- obser

    y <- cbind(param2,
               obser2,
               x[, c("tmax", "seed")])

    if (nrow(y) > 2 * n + interspace) {

      h <- head(y, n)
      t <- tail(y, n)
      hn <- rownames(h)
      tn <- rownames(t)
      m <- setNames(as.data.frame(matrix(".", interspace, ncol(y)),
                                  stringsAsFactors = FALSE), names(y))
      out <- rbind(h, m, t)
      out <- cbind(c(hn, rep(".", interspace), tn), out)
      names(out)[1] <- ""
      print(out, row.names = FALSE)

    } else print(y)

  }
  invisible(x)
}

# $<-.experiment ---------------------------------------------------------------

#' Replace a column of an experiment
#'
#' Replaces a column of an experiment with new value(s).
#'
#' If the length of the vector used to replace the column is not the same as the
#' original number of rows of the experiment, there is duplication of the
#' shortest element.
#'
#' @param exp An object of class \code{experiment}.
#' @param i A column index.
#' @param value A vector used to replace the values of the indexed column.
#'
#' @return An object of class \code{experiment}.
#'
#' @example /inst/examples/cash-set-.experiment.R
#' @export
`$<-.experiment` <- function(exp, i, value) {
  if (is.null(value)) NextMethod()
  else {
# Note: this code is very similar to the one of the fullfact function. Might be
# worth trying to optimize this in the future.
    to_expand <- as.data.frame(exp, stringsAsFactors = FALSE)
    the_names <- names(to_expand)
    to_expand <- c(
      to_expand[setdiff(the_names, i)], setNames(list(value), i))[the_names]
    new_exp <- do.call(expand.grid, lapply(to_expand, unique))
    # sort rows "from left to right"
    new_exp[do.call(order, new_exp), ]
    # add class and other attributes
    new_exp <- rbind(exp[1, ], new_exp)[-1, ]
    # regenerate row names
    row.names(new_exp) <- NULL
    new_exp
  }
}
