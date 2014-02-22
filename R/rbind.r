#' Efficiently rbind multiple data frames.
#'
#' This is an efficient version of the common pattern of
#' \code{do.call(rbind, dfs)} for row-binding many data frames together.
#' It works in the same way as \code{\link[plyr]{rbind.fill}} but is
#' implemented in C++ so avoids many copies and is much much faster.
#'
#' @param dots,... list of data frames to combine. With \code{rbind_all},
#'   they should already be in a list, with \code{rbind_list} you supply
#'   them individually.
#' @export
#' @rdname rbind
#' @examples
#' one <- mtcars[1:10, ]
#' two <- mtcars[11:32, ]
#'
#' rbind_list(one, two)
#' rbind_all(list(one, two))
#'
rbind_list <- function(...){
  rbind_list__impl(environment())  
}

