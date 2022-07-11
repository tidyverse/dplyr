test_that("`dplyr_locale()` uses the C locale by default", {
  expect_identical(dplyr_locale(), "C")
})

test_that("`dplyr_locale()` respects `dplyr.locale`", {
  local_options(dplyr.locale = "fr")
  expect_identical(dplyr_locale(), "fr")

  local_options(dplyr.locale = 1)
  expect_snapshot(error = TRUE, {
    dplyr_locale()
  })
})

test_that("`dplyr_legacy_locale()` is `FALSE` by default", {
  expect_false(dplyr_legacy_locale())
})

test_that("`dplyr_legacy_locale()` respects `dplyr.legacy_locale`", {
  local_options(dplyr.legacy_locale = TRUE)
  expect_true(dplyr_legacy_locale())

  local_options(dplyr.legacy_locale = 1)
  expect_snapshot(error = TRUE, {
    dplyr_legacy_locale()
  })
})
