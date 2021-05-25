#' Arrange rows by column values
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
#' ## Locales
#' TODO
#'
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

#' @param .by_group If `TRUE`, will sort first by grouping variable. Applies to
#'   grouped data frames only.
#' @param .locale The locale to sort character vectors in.
#'
#'   - If `NULL`, the default, character vectors are sorted in the American
#'     English locale as long as the stringi package is installed. If stringi
#'     is not available, the C locale will be used with a warning.
#'
#'   - If a single string is supplied, then this will be used as the locale
#'     identifier to sort with. For example, `"fr"` will sort with the French
#'     locale. This requires the stringi package. Use
#'     [stringi::stri_locale_list()] to generate a list of possible locale
#'     identifiers.
#'
#'   - If `"C"` is supplied, then character vectors will be sorted in the C
#'     locale. This does not require stringi and is often much faster than
#'     supplying a locale identifier.
#'
#' @rdname arrange
#' @export
arrange.data.frame <- function(.data, ..., .by_group = FALSE, .locale = NULL) {
  dots <- enquos(...)

  if (.by_group) {
    dots <- c(quos(!!!groups(.data)), dots)
  }

  chr_transform <- locale_to_chr_transform(.locale)

  loc <- arrange_rows(.data, dots, chr_transform)
  dplyr_row_slice(.data, loc)
}

# Helpers -----------------------------------------------------------------

arrange_rows <- function(.data, dots, chr_transform) {
  if (length(dots) == 0L) {
    out <- seq_len(nrow(.data))
    return(out)
  }

  directions <- map_chr(dots, function(quosure) {
    if(quo_is_call(quosure, "desc")) "desc" else "asc"
  })

  na_values <- if_else(directions == "desc", "smallest", "largest")

  quosures <- map(dots, function(quosure) {
    if (quo_is_call(quosure, "desc")) {
      quosure <- new_quosure(
        node_cadr(quo_get_expr(quosure)),
        quo_get_env(quosure)
      )
    }
    quosure
  })
  # give the quosures arbitrary names so that
  # data has the right number of columns below after transmute()
  names(quosures) <- paste0("^^--arrange_quosure_", seq_along(quosures))

  # TODO: not quite that because when the quosure is some expression
  #       it should be evaluated by groups.
  #       for now this abuses transmute so that we get just one
  #       column per quosure
  #
  #       revisit when we have something like mutate_one() to
  #       evaluate one quosure in the data mask
  data <- withCallingHandlers({
    transmute(new_data_frame(.data), !!!quosures)
  }, error = function(cnd) {

    if (inherits(cnd, "dplyr:::mutate_error")) {
      # reverse the name mangling
      bullets <- gsub("^^--arrange_quosure_", "..", cnd$bullets, fixed = TRUE)
    } else {
      bullets <- c(x = conditionMessage(cnd))
    }

    abort(c(
      "arrange() failed at implicit mutate() step. ",
      bullets
    ), class = "dplyr_error")

  })

  vec_order(
    x = data,
    direction = directions,
    na_value = na_values,
    chr_transform = chr_transform
  )
}

locale_to_chr_transform <- function(locale, has_stringi = has_minimum_stringi()) {
  if (is_null(locale)) {
    if (has_stringi) {
      return(sort_key_generator("en"))
    } else {
      warn_arrange_c_locale_fallback()
      return(NULL)
    }
  }

  if (identical(locale, "C")) {
    return(NULL)
  }

  if (is_character(locale)) {
    if (!is_string(locale)) {
      abort("If `.locale` is a character vector, it must be a single string.")
    }
    if (!has_stringi) {
      abort("stringi >= 1.5.3 is required to arrange in a different locale.")
    }
    if (!locale %in% stringi::stri_locale_list()) {
      abort("`.locale` must be one of the locales within `stringi::stri_locale_list()`.")
    }

    return(sort_key_generator(locale))
  }

  abort("`.locale` must be a string or `NULL`.")
}

warn_arrange_c_locale_fallback <- function() {
  warn(
    message = c(
      "stringi >= 1.5.3 is required to arrange in the American English locale.",
      i = "Falling back to the C locale.",
      i = "Silence this warning by supplying `.locale = \"C\"` or installing stringi."
    ),
    class = "dplyr_warn_arrange_c_locale_fallback"
  )
}
sort_key_generator <- function(locale) {
  function(x) {
    stringi::stri_sort_key(x, locale = locale)
  }
}
has_minimum_stringi <- function() {
  is_installed("stringi", version = "1.5.3")
}
