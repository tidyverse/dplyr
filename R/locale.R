#' Locale used by `arrange()`
#'
#' @description
#' This page documents details about the locale used by [arrange()] when
#' ordering character vectors.
#'
#' ## Default locale
#'
#' The default locale used by `arrange()` is the C locale. This is used when
#' `.locale = NULL` unless the deprecated `dplyr.legacy_locale` global option is
#' set to `TRUE`. You can also force the C locale to be used unconditionally
#' with `.locale = "C"`.
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
#' `r lifecycle::badge("deprecated")`
#'
#' Prior to dplyr 1.1.0, character columns were ordered in the system locale.
#' Setting the global option `dplyr.legacy_locale` to `TRUE` retains this legacy
#' behavior, but this has been deprecated. Update existing code to explicitly
#' call `arrange(.locale = )` instead. Run `Sys.getlocale("LC_COLLATE")` to
#' determine your system locale, and compare that against the list in
#' [stringi::stri_locale_list()] to find an appropriate value for `.locale`,
#' i.e. for American English, `"en_US"`.
#'
#' Setting `.locale` directly will override any usage of `dplyr.legacy_locale`.
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
NULL

dplyr_legacy_locale <- function() {
  # Used to determine if `group_by()` and `arrange()` should use
  # base R's `order()` for sorting, which respects the system locale and was
  # our sorting engine pre-1.1.0.
  option <- peek_option("dplyr.legacy_locale")

  if (is_null(option)) {
    # Default behavior uses C locale
    return(FALSE)
  }

  # This deprecation is a bit special. Since it is a global option that only the
  # end user would ever set, we set `user_env = globalenv()` so that it always
  # looks like a "direct" usage to lifecycle. This also makes our lives easier,
  # because `user_env` would have to be threaded all the way up through the
  # exported `grouped_df()` function, which is then used in many places
  # throughout dplyr. Additionally, we've bypassed `deprecate_soft()` and gone
  # straight to `deprecate_warn()` since this is only an end user facing option.
  lifecycle::deprecate_warn(
    when = "1.2.0",
    what = I("`options(dplyr.legacy_locale =)`"),
    details = c(
      i = "If needed for `arrange()`, use `arrange(.locale =)` instead.",
      i = "If needed for `group_by() |> summarise()`, follow up with an additional `arrange(.locale =)` call.",
      i = cli::format_inline(paste0(
        "Use {.run Sys.getlocale(\"LC_COLLATE\")} to determine your system locale, ",
        "and compare against {.run stringi::stri_locale_list()} to determine the `.locale` value to use."
      ))
    ),
    user_env = globalenv(),
    id = "dplyr-legacy-locale-option"
  )

  if (!is_bool(option)) {
    abort(
      "Global option `dplyr.legacy_locale` must be a single `TRUE` or `FALSE`.",
      call = NULL
    )
  }

  option
}

has_minimum_stringi <- function() {
  is_installed("stringi", version = "1.5.3")
}
