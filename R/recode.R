#' Recode values
#'
#' @description
#' This is a vectorised version of [switch()]: you can replace
#' numeric values based on their position or their name, and character or factor
#' values only by their name. This is an S3 generic: dplyr provides methods for
#' numeric, character, and factors. For logical vectors, use [if_else()]. For
#' more complicated criteria, use [case_when()].
#'
#' You can use `recode()` directly with factors; it will preserve the existing
#' order of levels while changing the values. Alternatively, you can
#' use `recode_factor()`, which will change the order of levels to match
#' the order of replacements. See the [forcats](http://forcats.tidyverse.org/)
#' package for more tools for working with factors and their levels.
#'
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("questioning")}
#' `recode()` is questioning because the arguments are in the wrong order.
#' We have `new <- old`, `mutate(df, new = old)`, and `rename(df, new = old)`
#' but `recode(x, old = new)`. We don't yet know how to fix this problem, but
#' it's likely to involve creating a new function then retiring or deprecating
#' `recode()`.
#'
#' @param .x A vector to modify
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Replacements. For character and factor `.x`, these should be named
#'   and replacement is based only on their name. For numeric `.x`, these can be
#'   named or not. If not named, the replacement is done based on position i.e.
#'   `.x` represents positions to look for in replacements. See examples.
#'
#'   When named, the argument names should be the current values to be replaced, and the
#'   argument values should be the new (replacement) values.
#'
#'   All replacements must be the same type, and must have either
#'   length one or the same length as `.x`.
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
#'   `recode_factor()` returns a factor whose levels are in the same order as
#'   in `...`. The levels in `.default` and `.missing` come last.
#' @seealso [na_if()] to replace specified values with a `NA`.
#'
#'   [coalesce()] to replace missing values with a specified value.
#'
#'   [tidyr::replace_na()] to replace `NA` with a value.
#' @export
#' @examples
#' # For character values, recode values with named arguments only. Unmatched
#' # values are unchanged.
#' char_vec <- sample(c("a", "b", "c"), 10, replace = TRUE)
#' recode(char_vec, a = "Apple")
#' recode(char_vec, a = "Apple", b = "Banana")
#'
#' # Use .default as replacement for unmatched values. Note that NA and
#' # replacement values need to be of the same type. For more information, see
#' # https://adv-r.hadley.nz/vectors-chap.html#missing-values
#' recode(char_vec, a = "Apple", b = "Banana", .default = NA_character_)
#'
#' # Throws an error as NA is logical, not character.
#' \dontrun{
#' recode(char_vec, a = "Apple", b = "Banana", .default = NA)
#' }
#'
#' # Use a named character vector for unquote splicing with !!!
#' level_key <- c(a = "apple", b = "banana", c = "carrot")
#' recode(char_vec, !!!level_key)
#'
#' # For numeric values, named arguments can also be used
#' num_vec <- c(1:4, NA)
#' recode(num_vec, `2` = 20L, `4` = 40L)
#'
#' # Or if you don't name the arguments, recode() matches by position.
#' # (Only works for numeric vector)
#' recode(num_vec, "a", "b", "c", "d")
#' # .x (position given) looks in (...), then grabs (... value at position)
#' # so if nothing at position (here 5), it uses .default or NA.
#' recode(c(1,5,3), "a", "b", "c", "d", .default = "nothing")
#'
#' # Note that if the replacements are not compatible with .x,
#' # unmatched values are replaced by NA and a warning is issued.
#' recode(num_vec, `2` = "b", `4` = "d")
#' # use .default to change the replacement value
#' recode(num_vec, "a", "b", "c", .default = "other")
#' # use .missing to replace missing values in .x
#' recode(num_vec, "a", "b", "c", .default = "other", .missing = "missing")
#'
#' # For factor values, use only named replacements
#' # and supply default with levels()
#' factor_vec <- factor(c("a", "b", "c"))
#' recode(factor_vec, a = "Apple", .default = levels(factor_vec))
#'
#' # Use recode_factor() to create factors with levels ordered as they
#' # appear in the recode call. The levels in .default and .missing
#' # come last.
#' recode_factor(num_vec, `1` = "z", `2` = "y", `3` = "x")
#' recode_factor(num_vec, `1` = "z", `2` = "y", `3` = "x",
#'               .default = "D")
#' recode_factor(num_vec, `1` = "z", `2` = "y", `3` = "x",
#'               .default = "D", .missing = "M")
#'
#' # When the input vector is a compatible vector (character vector or
#' # factor), it is reused as default.
#' recode_factor(letters[1:3], b = "z", c = "y")
#' recode_factor(factor(letters[1:3]), b = "z", c = "y")
#'
#' # Use a named character vector to recode factors with unquote splicing.
#' level_key <- c(a = "apple", b = "banana", c = "carrot")
#' recode_factor(char_vec, !!!level_key)
recode <- function(.x, ..., .default = NULL, .missing = NULL) {
  UseMethod("recode")
}

#' @export
recode.numeric <- function(.x, ..., .default = NULL, .missing = NULL) {
  values <- list2(...)

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
  .x <- as.character(.x)
  values <- list2(...)
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
  values <- list2(...)
  if (length(values) == 0) {
    abort("No replacements provided.")
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
    abort("No replacements provided.")
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
  values <- list2(...)
  recoded <- recode(.x, !!!values, .default = .default, .missing = .missing)

  all_levels <- unique(c(values, recode_default(.x, .default, recoded), .missing))
  recoded_levels <- if (is.factor(recoded)) levels(recoded) else unique(recoded)
  levels <- intersect(all_levels, recoded_levels)

  factor(recoded, levels, ordered = .ordered)
}
