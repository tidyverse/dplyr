#' Recode values
#'
#' This is a vectorised version of [switch()]: you can replace
#' numeric values based on their position, and character values by their
#' name. This is an S3 generic: dplyr provides methods for numeric, character,
#' and factors. For logical vectors, use [if_else()]. For more complicated
#' criteria, use [case_when()].
#'
#' You can use `recode()` directly with factors; it will preserve the existing
#' order of levels while changing the values. Alternatively, you can
#' use `recode_factor()`, which will change the order of levels to match
#' the order of replacements. See the [forcats](http://forcats.tidyverse.org/)
#' package for more tools for working with factors and their levels.
#'
#' @param .x A vector to modify
#' @param ... Replacements. These should be named for character and factor
#'   `.x`, and can be named for numeric `.x`. The argument names should be the
#'   current values to be replaced, and the argument values should be the new
#'   (replacement) values.
#'
#'   All replacements must be the same type, and must have either
#'   length one or the same length as x.
#'
#'   These dots are evaluated with [explicit splicing][rlang::dots_list].
#' @param .default If supplied, all values not otherwise matched will
#'   be given this value. If not supplied and if the replacements are
#'   the same type as the original values in `.x`, unmatched
#'   values are not changed. If not supplied and if the replacements
#'   are not compatible, unmatched values are replaced with `NA`.
#'
#'   `.default` must be either length 1 or the same length as
#'   `.x`.
#' @param .missing If supplied, any missing values in `.x` will be
#'   replaced by this value. Must be either length 1 or the same length as
#'   `.x`.
#' @param .ordered If `TRUE`, `recode_factor()` creates an
#'   ordered factor.
#' @return A vector the same length as `.x`, and the same type as
#'   the first of `...`, `.default`, or `.missing`.
#'   `recode_factor()` returns a factor whose levels are in the
#'   same order as in `...`.
#' @export
#' @examples
#' # Recode values with named arguments
#' x <- sample(c("a", "b", "c"), 10, replace = TRUE)
#' recode(x, a = "Apple")
#' recode(x, a = "Apple", .default = NA_character_)
#'
#' # Named arguments also work with numeric values
#' x <- c(1:5, NA)
#' recode(x, `2` = 20L, `4` = 40L)
#'
#' # Note that if the replacements are not compatible with .x,
#' # unmatched values are replaced by NA and a warning is issued.
#' recode(x, `2` = "b", `4` = "d")
#'
#' # If you don't name the arguments, recode() matches by position
#' recode(x, "a", "b", "c")
#' recode(x, "a", "b", "c", .default = "other")
#' recode(x, "a", "b", "c", .default = "other", .missing = "missing")
#'
#' # Supply default with levels() for factors
#' x <- factor(c("a", "b", "c"))
#' recode(x, a = "Apple", .default = levels(x))
#'
#' # Use recode_factor() to create factors with levels ordered as they
#' # appear in the recode call. The levels in .default and .missing
#' # come last.
#' x <- c(1:4, NA)
#' recode_factor(x, `1` = "z", `2` = "y", `3` = "x")
#' recode_factor(x, `1` = "z", `2` = "y", .default = "D")
#' recode_factor(x, `1` = "z", `2` = "y", .default = "D", .missing = "M")
#'
#' # When the input vector is a compatible vector (character vector or
#' # factor), it is reused as default.
#' recode_factor(letters[1:3], b = "z", c = "y")
#' recode_factor(factor(letters[1:3]), b = "z", c = "y")
recode <- function(.x, ..., .default = NULL, .missing = NULL) {
  UseMethod("recode")
}

#' @export
recode.numeric <- function(.x, ..., .default = NULL, .missing = NULL) {
  values <- dots_list(...)

  nms <- have_name(values)
  if (all(nms)) {
    vals <- as.double(names(values))
  } else if (all(!nms)) {
    vals <- seq_along(values)
  } else {
    abort("Either all values must be named, or none must be named.")
  }

  n <- length(.x)
  template <- find_template(values, .default, .missing)
  out <- template[rep(NA_integer_, n)]
  replaced <- rep(FALSE, n)

  for (i in seq_along(values)) {
    out <- replace_with(out, .x == vals[i], values[[i]], paste0("Vector ", i))
    replaced[.x == vals[i]] <- TRUE
  }

  .default <- validate_recode_default(.default, .x, out, replaced)
  out <- replace_with(out, !replaced & !is.na(.x), .default, "`.default`")
  out <- replace_with(out, is.na(.x), .missing, "`.missing`")
  out
}

#' @export
recode.character <- function(.x, ..., .default = NULL, .missing = NULL) {
  values <- dots_list(...)
  if (!all(have_name(values))) {
    bad <- which(!have_name(values)) + 1
    bad_pos_args(bad, "must be named, not unnamed")
  }

  n <- length(.x)
  template <- find_template(values, .default, .missing)
  out <- template[rep(NA_integer_, n)]
  replaced <- rep(FALSE, n)

  for (nm in names(values)) {
    out <- replace_with(out, .x == nm, values[[nm]], paste0("`", nm, "`"))
    replaced[.x == nm] <- TRUE
  }

  .default <- validate_recode_default(.default, .x, out, replaced)
  out <- replace_with(out, !replaced & !is.na(.x), .default, "`.default`")
  out <- replace_with(out, is.na(.x), .missing, "`.missing`")
  out
}

#' @export
recode.factor <- function(.x, ..., .default = NULL, .missing = NULL) {
  values <- dots_list(...)
  if (length(values) == 0) {
    abort("No replacements provided")
  }

  if (!all(have_name(values))) {
    bad <- which(!have_name(values)) + 1
    bad_pos_args(bad, "must be named, not unnamed")
  }
  if (!is.null(.missing)) {
    bad_args(".missing", "is not supported for factors")
  }

  n <- length(levels(.x))
  template <- find_template(values, .default, .missing)
  out <- template[rep(NA_integer_, n)]
  replaced <- rep(FALSE, n)

  for (nm in names(values)) {
    out <- replace_with(
      out,
      levels(.x) == nm,
      values[[nm]],
      paste0("`", nm, "`")
    )
    replaced[levels(.x) == nm] <- TRUE
  }
  .default <- validate_recode_default(.default, .x, out, replaced)
  out <- replace_with(out, !replaced, .default, "`.default`")

  if (is.character(out)) {
    levels(.x) <- out
    .x
  } else {
    out[as.integer(.x)]
  }

}

find_template <- function(values, .default = NULL, .missing = NULL) {
  x <- compact(c(values, .default, .missing))

  if (length(x) == 0) {
    abort("No replacements provided")
  }

  x[[1]]
}

validate_recode_default <- function(default, x, out, replaced) {
  default <- recode_default(x, default, out)

  if (is.null(default) && sum(replaced & !is.na(x)) < length(out[!is.na(x)])) {
    warning(
      "Unreplaced values treated as NA as .x is not compatible. ",
      "Please specify replacements exhaustively or supply .default",
      call. = FALSE
    )
  }

  default
}

recode_default <- function(x, default, out) {
  UseMethod("recode_default")
}

recode_default.default <- function(x, default, out) {
  same_type <- identical(typeof(x), typeof(out))
  if (is.null(default) && same_type) {
    x
  } else {
    default
  }
}

recode_default.factor <- function(x, default, out) {
  if (is.null(default)) {
    if ((is.character(out) || is.factor(out)) && is.factor(x)) {
      levels(x)
    } else {
      out[NA_integer_]
    }
  } else {
    default
  }
}

#' @rdname recode
#' @export
recode_factor <- function(.x, ..., .default = NULL, .missing = NULL,
                          .ordered = FALSE) {
  recoded <- recode(.x, ..., .default = .default, .missing = .missing)

  all_levels <- unique(c(..., recode_default(.x, .default, recoded), .missing))
  recoded_levels <- if (is.factor(recoded)) levels(recoded) else unique(recoded)
  levels <- intersect(all_levels, recoded_levels)

  factor(recoded, levels, ordered = .ordered)
}
