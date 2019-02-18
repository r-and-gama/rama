#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param y data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
anti_join.experiment <- function (x, y, by = NULL, copy = FALSE, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	old_attr <- c(old_attr, purrr::keep(attributes(y), names(attributes(y)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class"))) 
 	old_attr <- unique(old_attr) 
	y <- as.data.frame(y) 
	.data <- dplyr::anti_join(x, y, by = by, copy = copy, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
arrange.experiment <- function (.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::arrange(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @param .dots see corresponding function in package \code{dplyr} 
#' @name tidyverse 
arrange_.experiment <- function (.data, ..., .dots = list()) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::arrange_(.data, ..., .dots = .dots )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @name tidyverse 
arrange_all.experiment <- function (.tbl, .funs = list(), ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::arrange_all(.tbl, .funs = .funs, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .vars see corresponding function in package \code{dplyr} 
#' @param .funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @name tidyverse 
arrange_at.experiment <- function (.tbl, .vars, .funs = list(), ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::arrange_at(.tbl, .vars, .funs = .funs, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .predicate see corresponding function in package \code{dplyr} 
#' @param .funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @name tidyverse 
arrange_if.experiment <- function (.tbl, .predicate, .funs = list(), ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::arrange_if(.tbl, .predicate, .funs = .funs, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @param .keep_all see corresponding function in package \code{dplyr} 
#' @name tidyverse 
distinct.experiment <- function (.data, ..., .keep_all = FALSE) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::distinct(.data, ..., .keep_all = .keep_all )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @param .dots see corresponding function in package \code{dplyr} 
#' @param .keep_all see corresponding function in package \code{dplyr} 
#' @name tidyverse 
distinct_.experiment <- function (.data, ..., .dots, .keep_all = FALSE) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::distinct_(.data, ..., .dots, .keep_all = .keep_all )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
filter.experiment <- function (.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::filter(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @param .dots see corresponding function in package \code{dplyr} 
#' @name tidyverse 
filter_.experiment <- function (.data, ..., .dots = list()) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::filter_(.data, ..., .dots = .dots )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .vars_predicate see corresponding function in package \code{dplyr} 
#' @name tidyverse 
filter_all.experiment <- function (.tbl, .vars_predicate) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::filter_all(.tbl, .vars_predicate)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .vars see corresponding function in package \code{dplyr} 
#' @param .vars_predicate see corresponding function in package \code{dplyr} 
#' @name tidyverse 
filter_at.experiment <- function (.tbl, .vars, .vars_predicate) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::filter_at(.tbl, .vars, .vars_predicate)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .predicate see corresponding function in package \code{dplyr} 
#' @param .vars_predicate see corresponding function in package \code{dplyr} 
#' @name tidyverse 
filter_if.experiment <- function (.tbl, .predicate, .vars_predicate) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::filter_if(.tbl, .predicate, .vars_predicate)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param y data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
full_join.experiment <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"),     ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	old_attr <- c(old_attr, purrr::keep(attributes(y), names(attributes(y)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class"))) 
 	old_attr <- unique(old_attr) 
	y <- as.data.frame(y) 
	.data <- dplyr::full_join(x, y, by = by, copy = copy, suffix = suffix,     ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @param add see corresponding function in package \code{dplyr} 
#' @name tidyverse 
group_by.experiment <- function (.data, ..., add = FALSE) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::group_by(.data, ..., add = add )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @param .dots see corresponding function in package \code{dplyr} 
#' @param add see corresponding function in package \code{dplyr} 
#' @name tidyverse 
group_by_.experiment <- function (.data, ..., .dots = list(), add = FALSE) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::group_by_(.data, ..., .dots = .dots, add = add )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @name tidyverse 
group_by_all.experiment <- function (.tbl, .funs = list(), ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::group_by_all(.tbl, .funs = .funs, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .vars see corresponding function in package \code{dplyr} 
#' @param .funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @param .add see corresponding function in package \code{dplyr} 
#' @name tidyverse 
group_by_at.experiment <- function (.tbl, .vars, .funs = list(), ..., .add = FALSE) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::group_by_at(.tbl, .vars, .funs = .funs, ..., .add = .add )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .predicate see corresponding function in package \code{dplyr} 
#' @param .funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @param .add see corresponding function in package \code{dplyr} 
#' @name tidyverse 
group_by_if.experiment <- function (.tbl, .predicate, .funs = list(), ..., .add = FALSE) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::group_by_if(.tbl, .predicate, .funs = .funs, ..., .add = .add )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @param .dots see corresponding function in package \code{dplyr} 
#' @param add see corresponding function in package \code{dplyr} 
#' @name tidyverse 
group_by_prepare.experiment <- function (.data, ..., .dots = list(), add = FALSE) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::group_by_prepare(.data, ..., .dots = .dots, add = add )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
group_indices.experiment <- function (.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::group_indices(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @param .dots see corresponding function in package \code{dplyr} 
#' @name tidyverse 
group_indices_.experiment <- function (.data, ..., .dots = list()) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::group_indices_(.data, ..., .dots = .dots )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @name tidyverse 
group_size.experiment <- function (x) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	.data <- dplyr::group_size(x)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @name tidyverse 
group_vars.experiment <- function (x) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	.data <- dplyr::group_vars(x)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param data data object of class \link{experiment} 
#' @param vars see corresponding function in package \code{dplyr} 
#' @param drop see corresponding function in package \code{dplyr} 
#' @name tidyverse 
grouped_df.experiment <- function (data, vars, drop = TRUE) { 
	old_attr <- purrr::keep(attributes(data), names(attributes(data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	data <- as.data.frame(data) 
	.data <- dplyr::grouped_df(data, vars, drop = drop )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @name tidyverse 
groups.experiment <- function (x) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	.data <- dplyr::groups(x)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param y data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
inner_join.experiment <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"),     ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	old_attr <- c(old_attr, purrr::keep(attributes(y), names(attributes(y)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class"))) 
 	old_attr <- unique(old_attr) 
	y <- as.data.frame(y) 
	.data <- dplyr::inner_join(x, y, by = by, copy = copy, suffix = suffix,     ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @name tidyverse 
is_grouped_df.experiment <- function (x) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	.data <- dplyr::is_grouped_df(x)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @name tidyverse 
is.grouped_df.experiment <- function (x) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	.data <- dplyr::is.grouped_df(x)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param y data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
left_join.experiment <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"),     ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	old_attr <- c(old_attr, purrr::keep(attributes(y), names(attributes(y)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class"))) 
 	old_attr <- unique(old_attr) 
	y <- as.data.frame(y) 
	.data <- dplyr::left_join(x, y, by = by, copy = copy, suffix = suffix,     ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
mutate.experiment <- function (.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::mutate(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @param .dots see corresponding function in package \code{dplyr} 
#' @name tidyverse 
mutate_.experiment <- function (.data, ..., .dots = list()) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::mutate_(.data, ..., .dots = .dots )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @name tidyverse 
mutate_all.experiment <- function (.tbl, .funs, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::mutate_all(.tbl, .funs, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .vars see corresponding function in package \code{dplyr} 
#' @param .funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @param .cols see corresponding function in package \code{dplyr} 
#' @name tidyverse 
mutate_at.experiment <- function (.tbl, .vars, .funs, ..., .cols = NULL) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::mutate_at(.tbl, .vars, .funs, ..., .cols = .cols )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param tbl data object of class \link{experiment} 
#' @param funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @name tidyverse 
mutate_each.experiment <- function (tbl, funs, ...) { 
	old_attr <- purrr::keep(attributes(tbl), names(attributes(tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	tbl <- as.data.frame(tbl) 
	.data <- dplyr::mutate_each(tbl, funs, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param tbl data object of class \link{experiment} 
#' @param funs see corresponding function in package \code{dplyr} 
#' @param vars see corresponding function in package \code{dplyr} 
#' @name tidyverse 
mutate_each_.experiment <- function (tbl, funs, vars) { 
	old_attr <- purrr::keep(attributes(tbl), names(attributes(tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	tbl <- as.data.frame(tbl) 
	.data <- dplyr::mutate_each_(tbl, funs, vars)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .predicate see corresponding function in package \code{dplyr} 
#' @param .funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @name tidyverse 
mutate_if.experiment <- function (.tbl, .predicate, .funs, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::mutate_if(.tbl, .predicate, .funs, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @name tidyverse 
n_groups.experiment <- function (x) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	.data <- dplyr::n_groups(x)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param y data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
right_join.experiment <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"),     ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	old_attr <- c(old_attr, purrr::keep(attributes(y), names(attributes(y)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class"))) 
 	old_attr <- unique(old_attr) 
	y <- as.data.frame(y) 
	.data <- dplyr::right_join(x, y, by = by, copy = copy, suffix = suffix,     ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param tbl data object of class \link{experiment} 
#' @param size see corresponding function in package \code{dplyr} 
#' @param replace see corresponding function in package \code{dplyr} 
#' @param weight see corresponding function in package \code{dplyr} 
#' @param .env see corresponding function in package \code{dplyr} 
#' @name tidyverse 
sample_frac.experiment <- function (tbl, size = 1, replace = FALSE, weight = NULL, .env = NULL) { 
	old_attr <- purrr::keep(attributes(tbl), names(attributes(tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	tbl <- as.data.frame(tbl) 
	.data <- dplyr::sample_frac(tbl, size = size, replace = replace, weight = weight, .env = .env )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param tbl data object of class \link{experiment} 
#' @param size see corresponding function in package \code{dplyr} 
#' @param replace see corresponding function in package \code{dplyr} 
#' @param weight see corresponding function in package \code{dplyr} 
#' @param .env see corresponding function in package \code{dplyr} 
#' @name tidyverse 
sample_n.experiment <- function (tbl, size, replace = FALSE, weight = NULL, .env = NULL) { 
	old_attr <- purrr::keep(attributes(tbl), names(attributes(tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	tbl <- as.data.frame(tbl) 
	.data <- dplyr::sample_n(tbl, size, replace = replace, weight = weight, .env = .env )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
select.experiment <- function (.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::select(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @param .dots see corresponding function in package \code{dplyr} 
#' @name tidyverse 
select_.experiment <- function (.data, ..., .dots = list()) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::select_(.data, ..., .dots = .dots )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @name tidyverse 
select_all.experiment <- function (.tbl, .funs = list(), ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::select_all(.tbl, .funs = .funs, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .vars see corresponding function in package \code{dplyr} 
#' @param .funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @name tidyverse 
select_at.experiment <- function (.tbl, .vars, .funs = list(), ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::select_at(.tbl, .vars, .funs = .funs, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .predicate see corresponding function in package \code{dplyr} 
#' @param .funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @name tidyverse 
select_if.experiment <- function (.tbl, .predicate, .funs = list(), ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::select_if(.tbl, .predicate, .funs = .funs, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param y data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
semi_join.experiment <- function (x, y, by = NULL, copy = FALSE, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	old_attr <- c(old_attr, purrr::keep(attributes(y), names(attributes(y)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class"))) 
 	old_attr <- unique(old_attr) 
	y <- as.data.frame(y) 
	.data <- dplyr::semi_join(x, y, by = by, copy = copy, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
slice.experiment <- function (.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::slice(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @param .dots see corresponding function in package \code{dplyr} 
#' @name tidyverse 
slice_.experiment <- function (.data, ..., .dots = list()) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::slice_(.data, ..., .dots = .dots )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
summarise.experiment <- function (.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::summarise(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @param .dots see corresponding function in package \code{dplyr} 
#' @name tidyverse 
summarise_.experiment <- function (.data, ..., .dots = list()) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::summarise_(.data, ..., .dots = .dots )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @name tidyverse 
summarise_all.experiment <- function (.tbl, .funs, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::summarise_all(.tbl, .funs, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .vars see corresponding function in package \code{dplyr} 
#' @param .funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @param .cols see corresponding function in package \code{dplyr} 
#' @name tidyverse 
summarise_at.experiment <- function (.tbl, .vars, .funs, ..., .cols = NULL) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::summarise_at(.tbl, .vars, .funs, ..., .cols = .cols )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param tbl data object of class \link{experiment} 
#' @param funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @name tidyverse 
summarise_each.experiment <- function (tbl, funs, ...) { 
	old_attr <- purrr::keep(attributes(tbl), names(attributes(tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	tbl <- as.data.frame(tbl) 
	.data <- dplyr::summarise_each(tbl, funs, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param tbl data object of class \link{experiment} 
#' @param funs see corresponding function in package \code{dplyr} 
#' @param vars see corresponding function in package \code{dplyr} 
#' @name tidyverse 
summarise_each_.experiment <- function (tbl, funs, vars) { 
	old_attr <- purrr::keep(attributes(tbl), names(attributes(tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	tbl <- as.data.frame(tbl) 
	.data <- dplyr::summarise_each_(tbl, funs, vars)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .predicate see corresponding function in package \code{dplyr} 
#' @param .funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @name tidyverse 
summarise_if.experiment <- function (.tbl, .predicate, .funs, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::summarise_if(.tbl, .predicate, .funs, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @name tidyverse 
tbl_nongroup_vars.experiment <- function (x) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	.data <- dplyr::tbl_nongroup_vars(x)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
transmute.experiment <- function (.data, ...) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::transmute(.data, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .data data object of class \link{experiment} 
#' @param ... other arguments 
#' @param .dots see corresponding function in package \code{dplyr} 
#' @name tidyverse 
transmute_.experiment <- function (.data, ..., .dots = list()) { 
	old_attr <- purrr::keep(attributes(.data), names(attributes(.data)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.data <- as.data.frame(.data) 
	.data <- dplyr::transmute_(.data, ..., .dots = .dots )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @name tidyverse 
transmute_all.experiment <- function (.tbl, .funs, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::transmute_all(.tbl, .funs, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .vars see corresponding function in package \code{dplyr} 
#' @param .funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @param .cols see corresponding function in package \code{dplyr} 
#' @name tidyverse 
transmute_at.experiment <- function (.tbl, .vars, .funs, ..., .cols = NULL) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::transmute_at(.tbl, .vars, .funs, ..., .cols = .cols )
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param .tbl data object of class \link{experiment} 
#' @param .predicate see corresponding function in package \code{dplyr} 
#' @param .funs see corresponding function in package \code{dplyr} 
#' @param ... other arguments 
#' @name tidyverse 
transmute_if.experiment <- function (.tbl, .predicate, .funs, ...) { 
	old_attr <- purrr::keep(attributes(.tbl), names(attributes(.tbl)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	.tbl <- as.data.frame(.tbl) 
	.data <- dplyr::transmute_if(.tbl, .predicate, .funs, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

#' Tidyverse methods for experiment objects (remove .experiment suffix!) 
#' 
#' Tidyverse methods for experiment objects. Use these methods without the .experiment suffix and after loading the tidyverse package with the generic (or after loading package tidyverse). 
#' 
#' @param x data object of class \link{experiment} 
#' @param ... other arguments 
#' @name tidyverse 
ungroup.experiment <- function (x, ...) { 
	old_attr <- purrr::keep(attributes(x), names(attributes(x)) %in% c("dic_r2g", "dic_g2r", "wkdir", "experiment", "model", "class")) 
 	x <- as.data.frame(x) 
	.data <- dplyr::ungroup(x, ...)
 	attributes(.data) <- append( purrr::discard(attributes(.data), names(attributes(.data)) == "class"), old_attr) 
 	.data
 }

register_all_s3_methods <-  function() {
	 register_s3_method("dplyr", "anti_join", "experiment")
	 register_s3_method("dplyr", "arrange", "experiment")
	 register_s3_method("dplyr", "arrange_", "experiment")
	 register_s3_method("dplyr", "arrange_all", "experiment")
	 register_s3_method("dplyr", "arrange_at", "experiment")
	 register_s3_method("dplyr", "arrange_if", "experiment")
	 register_s3_method("dplyr", "distinct", "experiment")
	 register_s3_method("dplyr", "distinct_", "experiment")
	 register_s3_method("dplyr", "filter", "experiment")
	 register_s3_method("dplyr", "filter_", "experiment")
	 register_s3_method("dplyr", "filter_all", "experiment")
	 register_s3_method("dplyr", "filter_at", "experiment")
	 register_s3_method("dplyr", "filter_if", "experiment")
	 register_s3_method("dplyr", "full_join", "experiment")
	 register_s3_method("dplyr", "group_by", "experiment")
	 register_s3_method("dplyr", "group_by_", "experiment")
	 register_s3_method("dplyr", "group_by_all", "experiment")
	 register_s3_method("dplyr", "group_by_at", "experiment")
	 register_s3_method("dplyr", "group_by_if", "experiment")
	 register_s3_method("dplyr", "group_by_prepare", "experiment")
	 register_s3_method("dplyr", "group_indices", "experiment")
	 register_s3_method("dplyr", "group_indices_", "experiment")
	 register_s3_method("dplyr", "group_size", "experiment")
	 register_s3_method("dplyr", "group_vars", "experiment")
	 register_s3_method("dplyr", "grouped_df", "experiment")
	 register_s3_method("dplyr", "groups", "experiment")
	 register_s3_method("dplyr", "inner_join", "experiment")
	 register_s3_method("dplyr", "is_grouped_df", "experiment")
	 register_s3_method("dplyr", "is.grouped_df", "experiment")
	 register_s3_method("dplyr", "left_join", "experiment")
	 register_s3_method("dplyr", "mutate", "experiment")
	 register_s3_method("dplyr", "mutate_", "experiment")
	 register_s3_method("dplyr", "mutate_all", "experiment")
	 register_s3_method("dplyr", "mutate_at", "experiment")
	 register_s3_method("dplyr", "mutate_each", "experiment")
	 register_s3_method("dplyr", "mutate_each_", "experiment")
	 register_s3_method("dplyr", "mutate_if", "experiment")
	 register_s3_method("dplyr", "n_groups", "experiment")
	 register_s3_method("dplyr", "right_join", "experiment")
	 register_s3_method("dplyr", "sample_frac", "experiment")
	 register_s3_method("dplyr", "sample_n", "experiment")
	 register_s3_method("dplyr", "select", "experiment")
	 register_s3_method("dplyr", "select_", "experiment")
	 register_s3_method("dplyr", "select_all", "experiment")
	 register_s3_method("dplyr", "select_at", "experiment")
	 register_s3_method("dplyr", "select_if", "experiment")
	 register_s3_method("dplyr", "semi_join", "experiment")
	 register_s3_method("dplyr", "slice", "experiment")
	 register_s3_method("dplyr", "slice_", "experiment")
	 register_s3_method("dplyr", "summarise", "experiment")
	 register_s3_method("dplyr", "summarise_", "experiment")
	 register_s3_method("dplyr", "summarise_all", "experiment")
	 register_s3_method("dplyr", "summarise_at", "experiment")
	 register_s3_method("dplyr", "summarise_each", "experiment")
	 register_s3_method("dplyr", "summarise_each_", "experiment")
	 register_s3_method("dplyr", "summarise_if", "experiment")
	 register_s3_method("dplyr", "tbl_nongroup_vars", "experiment")
	 register_s3_method("dplyr", "transmute", "experiment")
	 register_s3_method("dplyr", "transmute_", "experiment")
	 register_s3_method("dplyr", "transmute_all", "experiment")
	 register_s3_method("dplyr", "transmute_at", "experiment")
	 register_s3_method("dplyr", "transmute_if", "experiment")
	 register_s3_method("dplyr", "ungroup", "experiment")
}


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
