#' Locale used by dplyr
#'
#' @description
#' `dplyr_locale()` returns a single string representing the default locale used
#' by dplyr when ordering character vectors. It is used as the default value of
#' `.locale` in [arrange()].
#'
#' ## Default locale
#'
#' - If stringi >= 1.5.3 is installed, the default locale is set to American
#'   English, represented by the locale identifier `"en"`.
#'
#' - If stringi is not installed or is older than 1.5.3, the default locale
#'   falls back to the C locale, represented by `"C"`. When this occurs, a
#'   warning will be thrown encouraging you to either install stringi, or
#'   replace usage of `dplyr_locale()` with `"C"` to explicitly force the C
#'   locale.
#'
#' ## Global override
#'
#' To override the above default behavior, you can set the global option,
#' `dplyr.locale`, to either `"C"` or a stringi locale identifier from
#' [stringi::stri_locale_list()] to globally alter the default locale.
#' Setting this option to anything other than `"C"` requires stringi >= 1.5.3.
#'
#' @export
dplyr_locale <- function() {
  locale <- peek_option("dplyr.locale")

  if (is_string(locale)) {
    return(locale)
  }
  if (!is_null(locale)) {
    abort("If set, the global option `dplyr.locale` must be a string.")
  }

  dplyr_locale_default()
}

dplyr_locale_default <- function(has_stringi = has_minimum_stringi()) {
  if (has_stringi) {
    "en"
  } else {
    warn_locale_fallback()
    "C"
  }
}

warn_locale_fallback <- function() {
  header <- paste0(
    "`dplyr_locale()` attempted to default to the American English locale (\"en\"), ",
    "but the required package, stringi >= 1.5.3, is not installed."
  )

  bullets <- c(
    i = "Falling back to the C locale.",
    i = paste0(
      "Silence this warning by installing stringi or by ",
      "explicitly replacing usage of `dplyr_locale()` with \"C\"."
    )
  )

  warn(
    message = c(header, bullets),
    class = "dplyr_warn_locale_fallback"
  )
}

has_minimum_stringi <- function() {
  is_installed("stringi", version = "1.5.3")
}
