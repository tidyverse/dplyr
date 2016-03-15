#' Recode values
#'
#' This is a vectorised version of \code{\link{switch}()}: you can replace
#' numeric values based on their position, and character values by their
#' name. This is an S3 generic: dplyr provides methods for numeric, character,
#' and factors. For logical vectors, use \code{\link{if_else}}
#'
#' @param x A vector to modify
#' @param ... Replacments. These should be named for character and factor
#'   \code{x}, and can be named for numeric \code{x}.
#'
#'   All replacements must be the same type, and must have either
#'   length one or the same length as x.
#' @param default If supplied, all values not otherwise matched will be
#'   given this value instead of \code{NA}. Must be either length 1 or the same
#'   length as \code{x}.
#' @param missing If supplied, any missing values in \code{x} will be
#'   replaced by this value. Must be either length 1 or the same length as
#'   \code{x}.
#' @return A vector the same length as \code{x}, and the same type as the
#'   first of \code{...}, \code{default}, or \code{missing}.
#' @export
#' @examples
#' x <- c(1:5, NA)
#' recode(x, "a", "b", "c")
#' recode(x, "a", "b", "c", default = "other")
#' recode(x, "a", "b", "c", default = "other", missing = "missing")
#' # Supply explicit values with named
#' recode(x, `2` = "b", `4` = "d")
#'
#' # Use named arguments with a character vector
#' x <- sample(c("a", "b", "c"), 10, replace = TRUE)
#' recode(x, a = "Apple")
#' recode(x, a = "Apple", default = x)
recode <- function(x, ..., default = NULL, missing = NULL) {
  UseMethod("recode")
}

#' @export
recode.numeric <- function(x, ..., default = NULL, missing = NULL) {
  values <- list(...)

  nms <- has_names(values)
  if (all(nms)) {
    vals <- as.double(names(values))
  } else if (all(!nms)) {
    vals <- seq_along(values)
  } else {
    stop("Either all values must be named, or none must be named.",
      call. = FALSE)
  }

  n <- length(x)
  template <- find_template(..., default, missing)
  out <- template[rep(NA_integer_, n)]
  replaced <- rep(FALSE, n)

  for (i in seq_along(values)) {
    out <- replace_with(out, x == vals[i], values[[i]], paste0("Vector ", i))
    replaced[x == i] <- TRUE
  }

  out <- replace_with(out, !replaced & !is.na(x), default, "`default`")
  out <- replace_with(out, is.na(x), missing, "`missing`")
  out
}

#' @export
recode.character <- function(x, ..., default = NULL, missing = NULL) {
  values <- list(...)
  if (!all(has_names(values))) {
    stop("All replacements must be named", call. = FALSE)
  }

  n <- length(x)
  template <- find_template(..., default, missing)
  out <- template[rep(NA_integer_, n)]
  replaced <- rep(FALSE, n)

  for (nm in names(values)) {
    out <- replace_with(out, x == nm, values[[nm]], paste0("`", nm, "`"))
    replaced[x == nm] <- TRUE
  }

  out <- replace_with(out, !replaced & !is.na(x), default, "`default`")
  out <- replace_with(out, is.na(x), missing, "`missing`")
  out
}

#' @export
recode.factor <- function(x, ..., default = NULL, missing = NULL) {
  values <- list(...)
  if (length(values) == 0) {
    stop("No replacements provided", call. = FALSE)
  }

  if (!all(has_names(values))) {
    stop("All replacements must be named", call. = FALSE)
  }
  if (!is.null(missing)) {
    stop("`missing` is not supported for factors", call. = FALSE)
  }

  out <- rep(NA_character_, length(levels(x)))
  replaced <- rep(FALSE, length(levels(x)))

  for (nm in names(values)) {
    out <- replace_with(out, levels(x) == nm, values[[nm]], paste0("`", nm, "`"))
    replaced[levels(x) == nm] <- TRUE
  }

  out <- replace_with(out, !replaced, default, "`default`")
  levels(x) <- out

  x
}

find_template <- function(...) {
  x <- compact(list(...))

  if (length(x) == 0) {
    stop("No replacements provided", call. = FALSE)
  }

  x[[1]]
}
