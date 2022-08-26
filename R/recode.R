#' Recode values
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' `recode()` is superseded in favor of [case_match()], which handles the most
#' important cases of `recode()` with a more elegant interface.
#' `recode_factor()` is also superseded, however, its direct replacement is not
#' currently available but will eventually live in
#' [forcats](https://forcats.tidyverse.org/). For creating new variables based
#' on logical vectors, use [if_else()]. For even more complicated criteria, use
#' [case_when()].
#'
#' `recode()` is a vectorised version of [switch()]: you can replace numeric
#' values based on their position or their name, and character or factor values
#' only by their name. This is an S3 generic: dplyr provides methods for
#' numeric, character, and factors. You can use `recode()` directly with
#' factors; it will preserve the existing order of levels while changing the
#' values. Alternatively, you can use `recode_factor()`, which will change the
#' order of levels to match the order of replacements.
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
#' char_vec <- sample(c("a", "b", "c"), 10, replace = TRUE)
#'
#' # `recode()` is superseded by `case_match()`
#' recode(char_vec, a = "Apple", b = "Banana")
#' case_match(char_vec, "a" ~ "Apple", "b" ~ "Banana", .default = char_vec)
#'
#' # With `case_match()`, you don't need typed missings like `NA_character_`
#' recode(char_vec, a = "Apple", b = "Banana", .default = NA_character_)
#' case_match(char_vec, "a" ~ "Apple", "b" ~ "Banana", .default = NA)
#'
#' # Throws an error as `NA` is logical, not character.
#' try(recode(char_vec, a = "Apple", b = "Banana", .default = NA))
#'
#' # `case_match()` is easier to use with numeric vectors, because you don't
#' # need to turn the numeric values into names
#' num_vec <- c(1:4, NA)
#' recode(num_vec, `2` = 20L, `4` = 40L)
#' case_match(num_vec, 2 ~ 20, 4 ~ 40, .default = num_vec)
#'
#' # `case_match()` doesn't have the ability to match by position like
#' # `recode()` does with numeric vectors
#' recode(num_vec, "a", "b", "c", "d")
#' recode(c(1,5,3), "a", "b", "c", "d", .default = "nothing")
#'
#' # For `case_match()`, incompatible types are an error rather than a warning
#' recode(num_vec, `2` = "b", `4` = "d")
#' try(case_match(num_vec, 2 ~ "b", 4 ~ "d", .default = num_vec))
#'
#' # The factor method of `recode()` can generally be replaced with
#' # `forcats::fct_recode()`
#' factor_vec <- factor(c("a", "b", "c"))
#' recode(factor_vec, a = "Apple")
#'
#' # `recode_factor()` does not currently have a direct replacement, but we
#' # plan to add one to forcats. In the meantime, you can use the `.ptype`
#' # argument to `case_match()`.
#' recode_factor(
#'   num_vec,
#'   `1` = "z",
#'   `2` = "y",
#'   `3` = "x",
#'   .default = "D",
#'   .missing = "M"
#' )
#' case_match(
#'   num_vec,
#'   1 ~ "z",
#'   2 ~ "y",
#'   3 ~ "x",
#'   NA ~ "M",
#'   .default = "D",
#'   .ptype = factor(levels = c("z", "y", "x", "D", "M"))
#' )
recode <- function(.x, ..., .default = NULL, .missing = NULL) {
  # Superseded in dplyr 1.1.0
  lifecycle::signal_stage("superseded", "recode()", "case_match()")
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
    msg <- "Either all values must be named, or none must be named."
    abort(msg)
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
    msg <- glue("{fmt_pos_args(bad)} must be named.")
    abort(msg)
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
    msg <- glue("{fmt_pos_args(bad)} must be named.")
    abort(msg)
  }
  if (!is.null(.missing)) {
    msg <- glue("`.missing` is not supported for factors.")
    abort(msg)
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

find_template <- function(values, .default = NULL, .missing = NULL, error_call = caller_env()) {
  x <- compact(c(values, .default, .missing))

  if (length(x) == 0) {
    abort("No replacements provided.", call = error_call)
  }

  x[[1]]
}

validate_recode_default <- function(default, x, out, replaced) {
  default <- recode_default(x, default, out)

  if (is.null(default) && sum(replaced & !is.na(x)) < length(out[!is.na(x)])) {
    bullets <- c(
      "Unreplaced values treated as NA as `.x` is not compatible. ",
      "Please specify replacements exhaustively or supply `.default`."
    )
    warn(bullets)
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
  # Superseded in dplyr 1.1.0
  lifecycle::signal_stage("superseded", "recode_factor()", I("`case_match(.ptype = factor(levels = ))`"))
  values <- list2(...)
  recoded <- recode(.x, !!!values, .default = .default, .missing = .missing)

  all_levels <- unique(c(values, recode_default(.x, .default, recoded), .missing))
  recoded_levels <- if (is.factor(recoded)) levels(recoded) else unique(recoded)
  levels <- intersect(all_levels, recoded_levels)

  factor(recoded, levels, ordered = .ordered)
}

# ------------------------------------------------------------------------------
# Helpers

replace_with <- function(x, i, val, name, reason = NULL, error_call = caller_env()) {
  if (is.null(val)) {
    return(x)
  }

  check_length(val, x, name, reason, error_call = error_call)
  check_type(val, x, name, error_call = error_call)
  check_class(val, x, name, error_call = error_call)

  i[is.na(i)] <- FALSE

  if (length(val) == 1L) {
    x[i] <- val
  } else {
    x[i] <- val[i]
  }

  x
}

fmt_check_length_val <- function(length_x, n, header, reason = NULL) {
  if (all(length_x %in% c(1L, n))) {
    return()
  }

  if (is.null(reason)) {
    reason <- ""
  } else {
    reason <- glue(" ({reason})")
  }

  if (n == 1) {
    glue("{header} must be length 1{reason}, not {commas(length_x)}.")
  } else {
    glue("{header} must be length {n}{reason} or one, not {commas(length_x)}.")
  }
}
check_length_val <- function(length_x, n, header, reason = NULL, error_call = caller_env()) {
  msg <- fmt_check_length_val(length_x, n, header, reason)
  if (length(msg)) {
    abort(msg, call = error_call)
  }
}

check_length <- function(x, template, header, reason = NULL, error_call = caller_env()) {
  check_length_val(length(x), length(template), header, reason, error_call = error_call)
}

check_type <- function(x, template, header, error_call = caller_env()) {
  if (identical(typeof(x), typeof(template))) {
    return()
  }

  msg <- glue("{header} must be {obj_type_friendly(template)}, not {obj_type_friendly(x)}.")
  abort(msg, call = error_call)
}

check_class <- function(x, template, header, error_call = caller_env()) {
  if (!is.object(x)) {
    return()
  }

  if (identical(class(x), class(template))) {
    return()
  }

  exp_classes <- fmt_classes(template)
  out_classes <- fmt_classes(x)
  msg <- glue("{header} must have class `{exp_classes}`, not class `{out_classes}`.")
  abort(msg, call = error_call)
}
