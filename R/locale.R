#' Locale used by dplyr
#'
#' @description
#' `dplyr_locale()` returns a single string representing the default locale used
#' by dplyr when ordering character vectors. It is used as the default value of
#' `.locale` in [arrange()].
#'
#' ## Default locale
#'
#' The default locale returned by `dplyr_locale()` is the C locale, identical
#' to explicitly supplying `.locale = "C"`.
#'
#' The C locale is not exactly the same as English locales, such as `"en"`. The
#' main difference is that the C locale groups the English alphabet by _case_,
#' while most English locales group the alphabet by _letter_. For example,
#' `c("a", "b", "C", "B", "c")` will sort as `c("B", "C", "a", "b", "c")` in the
#' C locale, with all uppercase letters coming before lowercase letters, but
#' will sort as `c("a", "b", "B", "c", "C")` in an English locale. This often
#' makes little practical difference during data analysis, because both return
#' identical results when case is consistent between observations.
#'
#' ## Global override
#'
#' To override the above default behavior, you can set the global option,
#' `dplyr.locale`, to a stringi locale identifier from
#' [stringi::stri_locale_list()] to globally alter the default locale. This
#' requires stringi >=1.5.3.
#'
#' We generally recommend that you set the `.locale` argument of [arrange()]
#' explicitly rather than overriding the global locale, if possible.
#'
#' Another alternative is to only change the global locale within a limited
#' scope through the use of [rlang::local_options()] or [rlang::with_options()].
#' This can be useful when a package that you don't control calls `arrange()`
#' internally.
#'
#' ## Reproducibility
#'
#' The C locale has the benefit of being completely reproducible across all
#' supported R versions and operating systems with no extra effort.
#'
#' If you set `.locale` to an option from [stringi::stri_locale_list()], then
#' stringi must be installed by anyone who wants to run your code. If you
#' utilize this in a package, then stringi should be placed in `Imports`.
#' @export
#' @keywords internal
#' @examplesIf dplyr:::has_minimum_stringi()
#' # Default locale is C
#' dplyr_locale()
#'
#' df <- tibble(x = c("a", "b", "C", "B", "c"))
#' df
#'
#' # The C locale groups the English alphabet by case, placing uppercase letters
#' # before lowercase letters. This is the default.
#' arrange(df, x)
#'
#' # The American English locale groups the alphabet by letter.
#' # Explicitly override `.locale` with `"en"` for this ordering.
#' arrange(df, x, .locale = "en")
#'
#' # Or temporarily override the `dplyr.locale` global option, which is useful
#' # if `arrange()` is called from a function you don't control
#' col_sorter <- function(df) {
#'   arrange(df, x)
#' }
#'
#' rlang::with_options(dplyr.locale = "en", {
#'   col_sorter(df)
#' })
#'
#' # This Danish letter is expected to sort after `z`
#' df <- tibble(x = c("o", "p", "\u00F8", "z"))
#' df
#'
#' # The American English locale sorts it right after `o`
#' arrange(df, x, .locale = "en")
#'
#' # Using `"da"` for Danish ordering gives the expected result
#' arrange(df, x, .locale = "da")
dplyr_locale <- function() {
  locale <- peek_option("dplyr.locale")

  if (is_string(locale)) {
    return(locale)
  }
  if (!is_null(locale)) {
    abort("If set, the global option `dplyr.locale` must be a string.")
  }

  "C"
}

has_minimum_stringi <- function() {
  is_installed("stringi", version = "1.5.3")
}

dplyr_legacy_locale <- function() {
  # Used to determine if `group_by()` and `arrange()` should use
  # base R's `order()` for sorting, which respects the system locale and was
  # our sorting engine pre-1.1.0.
  out <- peek_option("dplyr.legacy_locale") %||% FALSE

  if (!is_bool(out)) {
    abort(
      "Global option `dplyr.legacy_locale` must be a single `TRUE` or `FALSE`.",
      call = NULL
    )
  }

  out
}
