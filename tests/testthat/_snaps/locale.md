# `dplyr_legacy_locale()` respects `dplyr.legacy_locale`

    Code
      dplyr_legacy_locale()
    Condition
      Error:
      ! Global option `dplyr.legacy_locale` must be a single `TRUE` or `FALSE`.

# `dplyr_legacy_locale()` treats `dplyr.legacy_locale` as deprecated

    Code
      dplyr_legacy_locale()
    Condition
      Warning:
      `options(dplyr.legacy_locale =)` was deprecated in dplyr 1.2.0.
      i If needed for `arrange()`, use `arrange(.locale =)` instead.
      i If needed for `group_by() |> summarise()`, follow up with an additional `arrange(.locale =)` call.
      i Use `Sys.getlocale("LC_COLLATE")` to determine your system locale, and compare against `stringi::stri_locale_list()` to determine the `.locale` value to use.
    Output
      [1] TRUE

