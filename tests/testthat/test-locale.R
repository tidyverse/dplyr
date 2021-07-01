test_that("`dplyr_locale()` uses 'en' if stringi is available", {
  skip_if_not_installed("stringi", "1.5.3")

  expect_identical(dplyr_locale(), "en")
})

test_that("`dplyr_locale()` respects `tidyverse.locale_collation`", {
  local_options(tidyverse.locale_collation = "fr")
  expect_identical(dplyr_locale(), "fr")

  local_options(tidyverse.locale_collation = 1)
  expect_snapshot(error = TRUE, dplyr_locale())
})

test_that("`dplyr_locale()` falls back to the C locale with a warning if stringi is not available", {
  expect_snapshot(dplyr_locale_default(has_stringi = FALSE))
  expect_warning(dplyr_locale_default(has_stringi = FALSE), class = "dplyr_warn_locale_fallback")
})
