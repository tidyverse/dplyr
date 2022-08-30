#' Locale used by `arrange()`
#'
#' @description
#' This page documents details about the locale used by [arrange()] when
#' ordering character vectors.
#'
#' ## Default locale
#'
#' The default locale used by `arrange()` is the C locale. This is used when
#' `.locale = NULL` unless the `dplyr.legacy_locale` global option is set to
#' `TRUE`. You can also force the C locale to be used unconditionally with
#' `.locale = "C"`.
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
#' ## Reproducibility
#'
#' The C locale has the benefit of being completely reproducible across all
#' supported R versions and operating systems with no extra effort.
#'
#' If you set `.locale` to an option from [stringi::stri_locale_list()], then
#' stringi must be installed by anyone who wants to run your code. If you
#' utilize this in a package, then stringi should be placed in `Imports`.
#'
#' ## Legacy behavior
#'
#' Prior to dplyr 1.1.0, character columns were ordered in the system locale. If
#' you need to temporarily revert to this behavior, you can set the global
#' option `dplyr.legacy_locale` to `TRUE`, but this should be used sparingly and
#' you should expect this option to be removed in a future version of dplyr. It
#' is better to update existing code to explicitly use `.locale` instead. Note
#' that setting `dplyr.legacy_locale` will also force calls to [group_by()] to
#' use the system locale when internally ordering the groups.
#'
#' Setting `.locale` will override any usage of `dplyr.legacy_locale`.
#'
#' @name dplyr-locale
#' @keywords internal
#' @examplesIf dplyr:::has_minimum_stringi()
#' df <- tibble(x = c("a", "b", "C", "B", "c"))
#' df
#'
#' # Default locale is C, which groups the English alphabet by case, placing
#' # uppercase letters before lowercase letters.
#' arrange(df, x)
#'
#' # The American English locale groups the alphabet by letter.
#' # Explicitly override `.locale` with `"en"` for this ordering.
#' arrange(df, x, .locale = "en")
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
#'
#' # If you need the legacy behavior of `arrange()`, which respected the
#' # system locale, then you can set the global option `dplyr.legacy_locale`,
#' # but expect this to be removed in the future. We recommend that you use
#' # the `.locale` argument instead.
#' rlang::with_options(dplyr.legacy_locale = TRUE, {
#'   arrange(df, x)
#' })
NULL

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

has_minimum_stringi <- function() {
  is_installed("stringi", version = "1.5.3")
}
