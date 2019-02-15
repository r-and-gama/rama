# ------------------------------------------------------------------------------
body_function <- function(fct, argument, args1) {

  body_arg <- gsub("function ", "", argument) %>% strsplit(", ") %>% unlist %>%
    gsub("([[:alnum:]]*)( = )([[:alnum:]]*)", "\\1\\2\\1", .) %>%
    stringr::str_c(collapse = ", ")

  body_f <- NULL

  if (length(args1) > 0) {
    for (i in  1:length(args1)) {
      if (i > 1) {
        body_f <- paste0(
          body_f,
          '\told_attr <- c(old_attr, purrr::keep(attributes(', args1[i],
         '), names(attributes(', args1[i], ')) %in% ',
         'c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class"))) \n ',
         '\told_attr <- unique(old_attr) \n',
         '\t', args1[i], ' <- as.data.frame(', args1[i], ') \n')

      } else {
        body_f <- paste0(
          body_f,
          '\told_attr <- purrr::keep(attributes(', args1[i],
          '), names(attributes(', args1[i], ')) %in% ',
          'c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) \n ',
          '\t', args1[i], ' <- as.data.frame(', args1[i], ') \n')
      }
    }
    body_f
  }

  body_f <- paste0(body_f, '\t.data <- dplyr::', fct, body_arg, "\n \t")

  if (length(args1) > 0) {
    body_f <- paste0(
      body_f,
      'attributes(.data) <- append( purrr::discard(attributes(.data), ',
      'names(attributes(.data)) == "class"), old_attr) \n \t',".data")
    }
}

# ------------------------------------------------------------------------------
# arg1 : argument(s) of class experiment
doc_function <- function(args1, args2) {

  fct_doc <- paste0(
    "#' Tidyverse methods for experiment objects (remove .experiment suffix!) \n",
    "#' \n",
    "#' Tidyverse methods for experiment objects. Use these methods without the ",
    ".experiment suffix and after loading the tidyverse package with the generic ",
    "(or after loading package tidyverse). \n#' \n")
  if (length(args1) > 0) {
    for (i in  1:length(args1)) {
      fct_doc <- paste0(fct_doc, "#' @param ", args1[i],
                        " data object of class \\link{experiment} \n")
    }
  }
  fct_doc

  if (length(args2) > 0) {
    for (i in  1:length(args2)) {
      if (args2[i] == "...") {
        fct_doc <- paste0(fct_doc, "#' @param ... other arguments \n")
      } else {
        fct_doc <- paste0(fct_doc, "#' @param ", args2[i],
                          " see corresponding function in package \\code{dplyr} \n")
      }
    }
    fct_doc
  }
  fct_doc <- paste0(fct_doc, "#' @name tidyverse \n")
}

# ------------------------------------------------------------------------------
tidy_fct <- function(name) {
  # function argument
  argument <- capture.output(args(eval(sym(name))))
  argument <- gsub("NULL$", "", paste(argument, collapse = ""))
  argument_name <- names(formals(eval(sym(name))))
  args1 <- grep("\\.data|\\.tbl|^x$|^y$|^data$|^tbl$", argument_name, value = TRUE)
  args2 <- grep(".data|.tbl|x|y|data|tbl", argument_name, value = TRUE,
                invert = TRUE)

  # function
  fct_name <- paste0(name, ".experiment <- ", argument)
  body_f <- body_function(name, argument, args1)

  # function documentation
  doc_f <- doc_function(args1, args2)

  # total
  function_tot <- paste0(doc_f, fct_name, "{ \n", body_f, "\n }")
}

add_register_method <- function(lst_name) {
  lst <- lapply(lst_name, function(x) {
    paste0('\t register_s3_method("dplyr", "', x, '", "experiment")')
  })
  tot <- paste0("register_all_s3_methods <-  function() {\n",
                paste(lst, collapse = "\n"), "\n}\n")
}

# ------------------------------------------------------------------------------

# from: https://github.com/tidyverse/hms/blob/master/R/zzz.R
# Thu Apr 19 10:53:24 CEST 2018
#nocov start
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onAttach"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}

# ------------------------------------------------------------------------------
# The function `ls(getNamespaceInfo("dplyr", "exports"))` returns all the
# function exported by the package `dplyr`. In this list I only select the
# function that concern data frame and that the user may use to adapt for an
# object of class 'experiment'.
# I did not choose the function having a direct impact on the name of the column
# like the function 'rename' for example.
# I also choose the function based on the list of function that the package `sf`
# use.
fct <- ls(getNamespaceInfo("dplyr", "exports")) %>%
  grep(
    "sample|distinct|summarise|slice|transmute|group|arrange|filter|select|mutate|join",
    ., value = TRUE) %>%
  grep("sql|select_var|n_distinct", ., invert = TRUE, value = TRUE)

dplyr_fct <- lapply(fct, function(x) tidy_fct(x))
dplyr_fct <- append(dplyr_fct, add_register_method(fct))
dplyr_fct <- append(dplyr_fct,
                    paste0("register_s3_method <- ",
                      capture.output(eval(sym("register_s3_method"))) %>%
                        paste(collapse = "\n")))

writeLines(capture.output(cat(paste(dplyr_fct, collapse = "\n\n"))),
           con = paste0(getwd(), "/R/tidyverse.R"))

