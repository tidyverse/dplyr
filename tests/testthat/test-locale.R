test_that("`dplyr_legacy_locale()` is `FALSE` by default", {
  expect_false(dplyr_legacy_locale())
})

test_that("`dplyr_legacy_locale()` respects `dplyr.legacy_locale`", {
  local_options(lifecycle_verbosity = "quiet")

  local_options(dplyr.legacy_locale = TRUE)
  expect_true(dplyr_legacy_locale())

  local_options(dplyr.legacy_locale = 1)
  expect_snapshot(error = TRUE, {
    dplyr_legacy_locale()
  })
})

test_that("`dplyr_legacy_locale()` treats `dplyr.legacy_locale` as deprecated", {
  local_options(dplyr.legacy_locale = TRUE)

  expect_snapshot({
    dplyr_legacy_locale()
  })
})
