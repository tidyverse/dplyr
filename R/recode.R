#' Recode values
#'
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' `recode()` is superseded in favor of [recode_values()] and
#' [replace_values()], which are more general and have a much better interface.
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
#' @seealso [recode_values()]
#' @export
#' @examples
#' set.seed(1234)
#'
#' x <- sample(c("a", "b", "c"), 10, replace = TRUE)
#'
#' # `recode()` is superseded by `recode_values()` and `replace_values()`
#'
#' # If you are fully recoding a vector use `recode_values()`
#' recode(x, a = "Apple", b = "Banana", .default = NA_character_)
#' recode_values(x, "a" ~ "Apple", "b" ~ "Banana")
#'
#' # With a default
#' recode(x, a = "Apple", b = "Banana", .default = "unknown")
#' recode_values(x, "a" ~ "Apple", "b" ~ "Banana", default = "unknown")
#'
#' # If you are partially updating a vector and want to retain the original
#' # vector's values in locations you don't make a replacement, use
#' # `replace_values()`
#' recode(x, a = "Apple", b = "Banana")
#' replace_values(x, "a" ~ "Apple", "b" ~ "Banana")
#'
#' # `replace_values()` is easier to use with numeric vectors, because you don't
#' # need to turn the numeric values into names
#' y <- c(1:4, NA)
#' recode(y, `2` = 20L, `4` = 40L)
#' replace_values(y, 2 ~ 20L, 4 ~ 40L)
#'
#' # `recode()` is particularly confusing because it tries to handle both
#' # full recodings to new vector types and partial updating of an existing
#' # vector. With the above example, using doubles (20) rather than integers
#' # (20L) results in a warning from `recode()`, because it thinks you are
#' # doing a full recode and missed a case. `replace_values()` is type stable
#' # on `y` and will instead coerce the double values to integer.
#' recode(y, `2` = 20, `4` = 40)
#' replace_values(y, 2 ~ 20, 4 ~ 40)
#'
#' # This also makes `replace_values()` much safer. If you provide
#' # incompatible types, it will error.
#' recode(y, `2` = "20", `4` = "40")
#' try(replace_values(y, 2 ~ "20", 4 ~ "40"))
#'
#' # If you were trying to fully recode the vector and want a different output
#' # type, use `recode_values()`
#' recode_values(y, 2 ~ "20", 4 ~ "40")
#'
#' # And if you want to ensure you don't miss a case, use `unmatched`, which
#' # errors rather than warns
#' try(recode_values(y, 2 ~ "20", 4 ~ "40", unmatched = "error"))
#'
#' # ---------------------------------------------------------------------------
#' # Lookup tables
#'
#' # If you were splicing an external lookup vector into `recode()`, you can
#' # instead use the `from` and `to` arguments of `recode_values()`
#' x <- c("a", "b", "a", "c", "d", "c")
#'
#' lookup <- c(
#'   "a" = "A",
#'   "b" = "B",
#'   "c" = "C",
#'   "d" = "D"
#' )
#'
#' recode(x, !!!lookup)
#' recode_values(x, from = names(lookup), to = unname(lookup))
#'
#' # `recode_values()` is much more flexible here because the lookup table
#' # isn't restricted to just character values. We recommend using `tribble()`
#' # to build your lookup tables.
#' lookup <- tribble(
#'   ~from, ~to,
#'   "a", 1,
#'   "b", 2,
#'   "c", 3,
#'   "d", 4
#' )
#'
#' recode_values(x, from = lookup$from, to = lookup$to)
#'
#' # ---------------------------------------------------------------------------
#' # Factors
#'
#' # The factor method of `recode()` can generally be replaced with
#' # `forcats::fct_recode()`
#' x <- factor(c("a", "b", "c"))
#' recode(x, a = "Apple")
#' # forcats::fct_recode(x, "Apple" = "a")
#'
#' # `recode_factor()` does not currently have a direct replacement, but we
#' # plan to add one to forcats. In the meantime, use a lookup table that
#' # recodes every case, and then convert the `to` column to a factor. If you
#' # define your lookup table in your preferred level order, then the conversion
#' # to factor is straightforward!
#' y <- c(3, 4, 1, 2, 4, NA)
#'
#' recode_factor(
#'   y,
#'   `1` = "a",
#'   `2` = "b",
#'   `3` = "c",
#'   `4` = "d",
#'   .missing = "M"
#' )
#'
#' lookup <- tribble(
#'   ~from, ~to,
#'   1, "a",
#'   2, "b",
#'   3, "c",
#'   4, "d",
#'   NA, "M"
#' )
#' # `factor()` generates levels by sorting the unique values of `to`, which we
#' # don't want, so we supply `levels = to` directly. Alternatively, use
#' # `forcats::fct(to)`, which generates levels in order of appearance.
#' lookup <- mutate(lookup, to = factor(to, levels = to))
#'
#' recode_values(y, from = lookup$from, to = lookup$to)
recode <- function(.x, ..., .default = NULL, .missing = NULL) {
  # Superseded in dplyr 1.1.0
  lifecycle::signal_stage("superseded", "recode()", "recode_values()")
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

find_template <- function(
  values,
  .default = NULL,
  .missing = NULL,
  error_call = caller_env()
) {
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

#' @export
recode_default.default <- function(x, default, out) {
  same_type <- identical(typeof(x), typeof(out))
  if (is.null(default) && same_type) {
    x
  } else {
    default
  }
}

#' @export
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
recode_factor <- function(
  .x,
  ...,
  .default = NULL,
  .missing = NULL,
  .ordered = FALSE
) {
  # Superseded in dplyr 1.1.0
  lifecycle::signal_stage("superseded", "recode_factor()", "recode_values()")

  values <- list2(...)
  recoded <- recode(.x, !!!values, .default = .default, .missing = .missing)

  all_levels <- unique(c(
    values,
    recode_default(.x, .default, recoded),
    .missing
  ))
  recoded_levels <- if (is.factor(recoded)) levels(recoded) else unique(recoded)
  levels <- intersect(all_levels, recoded_levels)

  factor(recoded, levels, ordered = .ordered)
}

# ------------------------------------------------------------------------------
# Helpers

replace_with <- function(
  x,
  i,
  val,
  name,
  reason = NULL,
  error_call = caller_env()
) {
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
check_length_val <- function(
  length_x,
  n,
  header,
  reason = NULL,
  error_call = caller_env()
) {
  msg <- fmt_check_length_val(length_x, n, header, reason)
  if (length(msg)) {
    abort(msg, call = error_call)
  }
}

check_length <- function(
  x,
  template,
  header,
  reason = NULL,
  error_call = caller_env()
) {
  check_length_val(
    length(x),
    length(template),
    header,
    reason,
    error_call = error_call
  )
}

check_type <- function(x, template, header, error_call = caller_env()) {
  if (identical(typeof(x), typeof(template))) {
    return()
  }

  msg <- glue(
    "{header} must be {obj_type_friendly(template)}, not {obj_type_friendly(x)}."
  )
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
  msg <- glue(
    "{header} must have class `{exp_classes}`, not class `{out_classes}`."
  )
  abort(msg, call = error_call)
}
