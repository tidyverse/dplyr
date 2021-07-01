# `dplyr_locale()` respects `tidyverse.locale_collation`

    Code
      dplyr_locale()
    Error <rlang_error>
      If set, the global option `tidyverse.locale_collation` must be a string.

# `dplyr_locale()` falls back to the C locale with a warning if stringi is not available

    Code
      dplyr_locale_default(has_stringi = FALSE)
    Warning <dplyr_warn_locale_fallback>
      `dplyr_locale()` attempted to default to the American English locale ("en"), but the required package, stringi >= 1.5.3, is not installed.
      i Falling back to the C locale.
      i Silence this warning by installing stringi or by explicitly replacing usage of `dplyr_locale()` with "C".
    Output
      [1] "C"

