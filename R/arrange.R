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
#' @param ... <[`data-masking`][dplyr_data_masking]> Variables, or functions of
#'   variables. Use [desc()] to sort a variable in descending order.
#' @param .by_group If `TRUE`, will sort first by grouping variable. Applies to
#'   grouped data frames only.
#' @param .locale The locale to sort character vectors in.
#'
#'   - Defaults to [dplyr_locale()], which uses the `"C"` locale unless this is
#'     explicitly overridden. See the help page for [dplyr_locale()] for the
#'     exact details.
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
#'   letters. This is explained in more detail in the help page of
#'   [dplyr_locale()] under the `Default locale` section.
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
#' # see ?dplyr_data_masking for more details
#' tidy_eval_arrange <- function(.data, var) {
#'   .data %>%
#'     arrange({{ var }})
#' }
#' tidy_eval_arrange(mtcars, mpg)
#'
#' # use across() access select()-style semantics
#' iris %>% arrange(across(starts_with("Sepal")))
#' iris %>% arrange(across(starts_with("Sepal"), desc))
arrange <- function(.data, ..., .by_group = FALSE) {
  UseMethod("arrange")
}

#' @rdname arrange
#' @export
arrange.data.frame <- function(.data,
                               ...,
                               .by_group = FALSE,
                               .locale = dplyr_locale()) {
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
  error_call <- dplyr_error_call(error_call)

  chr_proxy_collate <- locale_to_chr_proxy_collate(
    locale = locale,
    error_call = error_call
  )

  if (length(dots) == 0L) {
    out <- seq_len(nrow(data))
    return(out)
  }

  # Strip out calls to desc() replacing with direction argument
  is_desc_call <- function(x) {
    quo_is_call(x, "desc", ns = c("", "dplyr"))
  }
  directions <- map_chr(dots, function(quosure) {
    if (is_desc_call(quosure)) "desc" else "asc"
  })
  quosures <- map(dots, function(quosure) {
    if (is_desc_call(quosure)) {
      expr <- quo_get_expr(quosure)
      if (!has_length(expr, 2L)) {
        abort("`desc()` must be called with exactly one argument.", call = error_call)
      }

      quosure <- new_quosure(node_cadr(expr), quo_get_env(quosure))
    }
    quosure
  })

  # give the quosures arbitrary names so that
  # data has the right number of columns below after transmute()
  quo_names <- paste0("^^--arrange_quosure_", seq_along(quosures))
  names(quosures) <- quo_names

  # TODO: not quite that because when the quosure is some expression
  #       it should be evaluated by groups.
  #       for now this abuses transmute so that we get just one
  #       column per quosure
  #
  #       revisit when we have something like mutate_one() to
  #       evaluate one quosure in the data mask
  data <- withCallingHandlers({
    transmute(new_data_frame(data), !!!quosures)
  }, error = function(cnd) {

    if (inherits(cnd, "dplyr:::mutate_error")) {
      # reverse the name mangling
      bullets <- gsub("^^--arrange_quosure_", "..", cnd$bullets, fixed = TRUE)
      # only name bullets that aren't already named
      names <- names2(bullets)
      names[names == ""] <- "x"
      bullets <- set_names(bullets, names)

      # skip the parent as this has reworked the bullets
      # and this would be confusing to have them
      parent <- cnd$parent
    } else {
      parent <- cnd
      bullets <- c()
    }

    bullets <- c(
      "Problem with the implicit `transmute()` step. ",
      bullets
    )
    abort(bullets, call = error_call, parent = parent)

  })

  directions <- directions[quo_names %in% names(data)]
  na_values <- if_else(directions == "desc", "smallest", "largest")

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

  if (identical(locale, "C")) {
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

  abort("`.locale` must be a string.", call = error_call)
}

sort_key_generator <- function(locale) {
  function(x) {
    stringi::stri_sort_key(x, locale = locale)
  }
}
