#' Order rows using column values
#'
#' @description
#' `arrange()` orders the rows of a data frame by the values of selected
#' columns.
#'
#' Unlike other dplyr verbs, `arrange()` largely ignores grouping; you
#' need to explicitly mention grouping variables (or use  `.by_group = TRUE`)
#' in order to group by them, and functions of variables are evaluated
#' once per data frame, not once per group.
#'
#' @details
#' ## Missing values
#' Unlike base sorting with `sort()`, `NA` are:
#' * always sorted to the end for local data, even when wrapped with `desc()`.
#' * treated differently for remote data, depending on the backend.
#'
#' @return
#' An object of the same type as `.data`. The output has the following
#' properties:
#'
#' * All rows appear in the output, but (usually) in a different place.
#' * Columns are not modified.
#' * Groups are not modified.
#' * Data frame attributes are preserved.
#' @section Methods:
#' This function is a **generic**, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' The following methods are currently available in loaded packages:
#' \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("arrange")}.
#' @export
#' @param .data A data frame, data frame extension (e.g. a tibble), or a
#'   lazy data frame (e.g. from dbplyr or dtplyr). See *Methods*, below, for
#'   more details.
#' @param ... <[`data-masking`][rlang::args_data_masking]> Variables, or
#'   functions of variables. Use [desc()] to sort a variable in descending
#'   order.
#' @param .by_group If `TRUE`, will sort first by grouping variable. Applies to
#'   grouped data frames only.
#' @param .locale The locale to sort character vectors in.
#'
#'   - If `NULL`, the default, uses the `"C"` locale unless the
#'     `dplyr.legacy_locale` global option escape hatch is active. See the
#'     [dplyr-locale] help page for more details.
#'
#'   - If a single string from [stringi::stri_locale_list()] is supplied, then
#'     this will be used as the locale to sort with. For example, `"en"` will
#'     sort with the American English locale. This requires the stringi package.
#'
#'   - If `"C"` is supplied, then character vectors will always be sorted in the
#'     C locale. This does not require stringi and is often much faster than
#'     supplying a locale identifier.
#'
#'   The C locale is not the same as English locales, such as `"en"`,
#'   particularly when it comes to data containing a mix of upper and lower case
#'   letters. This is explained in more detail on the [locale][dplyr-locale]
#'   help page under the `Default locale` section.
#' @family single table verbs
#' @examples
#' arrange(mtcars, cyl, disp)
#' arrange(mtcars, desc(disp))
#'
#' # grouped arrange ignores groups
#' by_cyl <- mtcars %>% group_by(cyl)
#' by_cyl %>% arrange(desc(wt))
#' # Unless you specifically ask:
#' by_cyl %>% arrange(desc(wt), .by_group = TRUE)
#'
#' # use embracing when wrapping in a function;
#' # see ?rlang::args_data_masking for more details
#' tidy_eval_arrange <- function(.data, var) {
#'   .data %>%
#'     arrange({{ var }})
#' }
#' tidy_eval_arrange(mtcars, mpg)
#'
#' # Use `across()` or `pick()` to select columns with tidy-select
#' iris %>% arrange(pick(starts_with("Sepal")))
#' iris %>% arrange(across(starts_with("Sepal"), desc))
arrange <- function(.data, ..., .by_group = FALSE) {
  UseMethod("arrange")
}

#' @rdname arrange
#' @export
arrange.data.frame <- function(.data,
                               ...,
                               .by_group = FALSE,
                               .locale = NULL) {
  dots <- enquos(...)

  if (.by_group) {
    dots <- c(quos(!!!groups(.data)), dots)
  }

  loc <- arrange_rows(.data, dots = dots, locale = .locale)
  dplyr_row_slice(.data, loc)
}

# Helpers -----------------------------------------------------------------

arrange_rows <- function(data,
                         dots,
                         locale,
                         error_call = caller_env()) {
  dplyr_local_error_call(error_call)

  size <- nrow(data)

  # `arrange()` implementation always uses bare data frames
  data <- new_data_frame(data, n = size)

  # Strip out calls to desc() replacing with direction argument
  is_desc_call <- function(x) {
    quo_is_call(x, "desc", ns = c("", "dplyr"))
  }
  directions <- map_chr(dots, function(dot) {
    if (is_desc_call(dot)) "desc" else "asc"
  })
  dots <- map(dots, function(dot) {
    if (is_desc_call(dot)) {
      expr <- quo_get_expr(dot)
      if (!has_length(expr, 2L)) {
        abort("`desc()` must be called with exactly one argument.", call = error_call)
      }

      dot <- new_quosure(expr[[2]], quo_get_env(dot))
    }
    dot
  })

  n_dots <- length(dots)
  seq_dots <- seq_len(n_dots)

  cols <- vector("list", length = n_dots)
  names(cols) <- as.character(seq_dots)

  for (i in seq_dots) {
    name <- vec_paste0("..", i)
    dot <- dots[[i]]

    # Evaluate each `dot` on the original data separately, rather than
    # evaluating all at once. We want to avoid the "sequential evaluation"
    # feature of `mutate()` where the 2nd expression can access results of the
    # 1st (#6495).
    elt <- mutate(data, "{name}" := !!dot, .keep = "none")
    elt <- elt[[name]]

    if (is.null(elt)) {
      # In this case, `dot` evaluated to `NULL` for "column removal" so
      # `elt[[name]]` won't exist, but we don't want to shorten `cols`.
      next
    }

    cols[[i]] <- elt
  }

  if (vec_any_missing(cols)) {
    # Drop `NULL`s that could result from `dot` evaluating to `NULL` above
    not_missing <- !vec_detect_missing(cols)
    cols <- vec_slice(cols, not_missing)
    directions <- vec_slice(directions, not_missing)
  }

  data <- new_data_frame(cols, n = size)

  if (is.null(locale) && dplyr_legacy_locale()) {
    # Temporary legacy support for respecting the system locale.
    # Only applied when `.locale` is `NULL` and `dplyr.legacy_locale` is set.
    # Matches legacy `group_by()` ordering.
    out <- dplyr_order_legacy(data = data, direction = directions)
    return(out)
  }

  na_values <- if_else(directions == "desc", "smallest", "largest")

  chr_proxy_collate <- locale_to_chr_proxy_collate(
    locale = locale,
    error_call = error_call
  )

  vec_order_radix(
    x = data,
    direction = directions,
    na_value = na_values,
    chr_proxy_collate = chr_proxy_collate
  )
}

locale_to_chr_proxy_collate <- function(locale,
                                        ...,
                                        has_stringi = has_minimum_stringi(),
                                        error_call = caller_env()) {
  check_dots_empty0(...)

  if (is.null(locale) || is_string(locale, string = "C")) {
    return(NULL)
  }

  if (is_character(locale)) {
    if (!is_string(locale)) {
      abort("If `.locale` is a character vector, it must be a single string.", call = error_call)
    }
    if (!has_stringi) {
      abort("stringi >=1.5.3 is required to arrange in a different locale.", call = error_call)
    }
    if (!locale %in% stringi::stri_locale_list()) {
      abort("`.locale` must be one of the locales within `stringi::stri_locale_list()`.", call = error_call)
    }

    return(sort_key_generator(locale))
  }

  abort("`.locale` must be a string or `NULL`.", call = error_call)
}

sort_key_generator <- function(locale) {
  function(x) {
    stringi::stri_sort_key(x, locale = locale)
  }
}

# ------------------------------------------------------------------------------

dplyr_order_legacy <- function(data, direction = "asc") {
  if (ncol(data) == 0L) {
    # Work around `order(!!!list())` returning `NULL`
    return(seq_len(nrow(data)))
  }

  proxies <- map2(data, direction, dplyr_proxy_order_legacy)
  proxies <- unname(proxies)

  inject(order(!!!proxies))
}

dplyr_proxy_order_legacy <- function(x, direction) {
  # `order()` doesn't have a vectorized `decreasing` argument for most values of
  # `method` ("radix" is an exception). So we need to apply this by column ahead
  # of time. We have to apply `vec_proxy_order()` by column too, rather than on
  # the original data frame, because it flattens df-cols and we can lose track
  # of where to apply `direction`.
  x <- vec_proxy_order(x)

  if (is.data.frame(x)) {
    if (any(map_lgl(x, is.data.frame))) {
      abort(
        "All data frame columns should have been flattened by now.",
        .internal = TRUE
      )
    }

    # Special handling for data frame proxies (either from df-cols or from
    # vector classes with df proxies, like rcrds), which `order()` can't handle.
    # We have to replace the df proxy with a single vector that orders the same
    # way, so we use a dense rank that utilizes the system locale.
    unique <- vec_unique(x)
    order <- dplyr_order_legacy(unique, direction)
    sorted_unique <- vec_slice(unique, order)
    out <- vec_match(x, sorted_unique)

    return(out)
  }

  if (!is_character(x) && !is_logical(x) && !is_integer(x) && !is_double(x) && !is_complex(x)) {
    abort("Invalid type returned by `vec_proxy_order()`.", .internal = TRUE)
  }

  if (is.object(x)) {
    x <- unstructure(x)
  }

  if (direction == "desc") {
    x <- desc(x)
  }

  x
}
