test_that("can update nothing", {
  df <- tibble(x = c(1, 2, 3), y = c(4, 5, 6))

  expect_identical(mutate(df, .when = x >= 2), df)
})

test_that("grouped - can update nothing", {
  df <- tibble(x = c(1, 2, 3), y = c(4, 5, 6), g = c(1, 1, 2))
  df <- group_by(df, g)

  expect_identical(mutate(df, .when = x >= 2), df)
})

test_that("rowwise - can update nothing", {
  df <- tibble(x = c(1, 2, 3), y = c(4, 5, 6))
  df <- rowwise(df)

  expect_identical(mutate(df, .when = x >= 2), df)
})

test_that("can update with a constant", {
  df <- tibble(x = c(1, 2, 3), y = c(4, 5, 6))

  out <- mutate(df, .when = x >= 2, y = 1)
  expect_identical(out$y, c(4, 1, 1))
})

test_that("grouped - can update with a constant", {
  df <- tibble(x = c(1, 2, 3), y = c(4, 5, 6), g = c(1, 1, 2))
  df <- group_by(df, g)

  out <- mutate(df, .when = x >= 2, y = 1)
  expect_identical(out$y, c(4, 1, 1))
})

test_that("rowwise - can update with a constant", {
  df <- tibble(x = c(1, 2, 3), y = c(4, 5, 6))
  df <- rowwise(df)

  out <- mutate(df, .when = x >= 2, y = 1)
  expect_identical(out$y, c(4, 1, 1))
})

test_that("can update with an inlined vector the same size as where `.when` is `TRUE`", {
  # A bit silly, but matches the current behavior of `!!` within `mutate()`
  df <- tibble(x = c(1, 2, 3), y = c(4, 5, 6))

  z <- c(7, 8)

  out <- mutate(df, .when = x >= 2, y = !!z)

  expect_identical(out$y, c(4, 7, 8))
})

test_that("grouped - can update with an inlined vector the same size as where `.when` is `TRUE`", {
  # A bit silly, but matches the current behavior of `!!` within `mutate()`
  df <- tibble(g = c(1, 1, 1, 2, 2, 2), x = c(1, 2, 3, 1, 2, 3), y = 5)
  df <- group_by(df, g)

  z <- c(6, 7, 8, 9)

  # `x >= 2` is `TRUE` in 4 locations, and ignores grouping, so `z` must be size 4
  out <- mutate(df, .when = x >= 2, y = !!z)

  expect_identical(out$y, c(5, 6, 7, 5, 8, 9))
})

test_that("can update using another column by referencing its name", {
  df <- tibble(x = c(1, 2, 3), y = c(4, 5, 6))

  out <- mutate(df, .when = x >= 2, y = x)

  expect_identical(out$y, c(4, 2, 3))
})

test_that("can update using an expression involving multiple columns", {
  df <- tibble(x = c(1, 2, 3), y = c(4, 5, 6), z = c(7, 8, 9))

  out <- mutate(df, .when = x >= 2, y = y + z)

  expect_identical(out$y, c(4, 13, 15))
})

test_that("can update multiple columns", {
  df <- tibble(x = c(NA, 2, NA), y = c(4, 5, 6), z = c(7, 8, 9))

  out <- mutate(df, .when = is.na(x), y = NA, z = 100)

  expect_identical(out$y, c(NA, 5, NA))
  expect_identical(out$z, c(100, 8, 100))
})

test_that("can repeatedly update the same column", {
  df <- tibble(x = c(NA, 2, NA), y = c(4, 5, 6))

  out <- mutate(df, .when = is.na(x), y = 100, y = y + 1)

  expect_identical(out$y, c(101, 5, 101))
})

test_that("treats `NA` in `.when` like `FALSE`", {
  df <- tibble(x = c(NA, 2, NA), y = c(4, 5, 6))

  out <- mutate(df, .when = x == 2, y = x)

  expect_identical(out$y, c(4, 2, 6))
})

test_that("grouped - column expressions are computed per group", {
  df <- tibble(x = c(1, 2, 2, 3), y = c(3, 4, 5, 6), g = c(1, 1, 2, 2))
  df <- group_by(df, g)

  out <- mutate(df, .when = x >= 2, y = mean(x))
  expect_identical(out$y, c(3, 2, 2.5, 2.5))
})

test_that("rowwise - accesses elements of list columns", {
  df <- tibble(x = c(1, 2, 3), y = list(4, 5, 6))
  df <- rowwise(df)

  out <- mutate(df, .when = x >= 2, z = y + 1)
  expect_identical(out$z, c(NA, 6, 7))
})

test_that("rowwise - doesn't run on `.when = FALSE` locations", {
  df <- tibble(x = c(1, 2, 3), y = list(4, 5, 6))
  df <- rowwise(df)

  out <- mutate(df, .when = x >= 2, z = if(y == 4) stop("oh no") else 1)
  expect_identical(out$z, c(NA, 1, 1))
})

test_that("grouped - doesn't evaluate on groups that are filtered out by `.when`", {
  df <- tibble(g = c(1, 1, 2, 2), x = c(1, 2, 3, 4))
  df <- group_by(df, g)

  # Matches SQL's WHERE behavior and data.table's `i + by` behavior.
  # After applying `.when`, we get a grouping structure like:
  # list(c(3L, 4L))
  # Not like:
  # list(integer(), c(3L, 4L))
  out <- mutate(df, .when = x > 2, y = if (length(x) == 0) stop("oh no") else 1)

  expect_identical(out$y, c(NA, NA, 1, 1))
})

test_that("applies expression even when `.when` excludes all rows", {
  df <- tibble(x = c(1, 2, 3, 4))

  # Constant
  out <- mutate(df, .when = x > 5, y = 1L)
  expect_identical(out$y, rep(NA_integer_, 4))

  # Column
  out <- mutate(df, .when = x > 5, y = x)
  expect_identical(out$y, rep(NA_real_, 4))

  # Expression
  out <- mutate(df, .when = x > 5, y = x + 1)
  expect_identical(out$y, rep(NA_real_, 4))
})

test_that("grouped - applies expression even when `.when` excludes all rows", {
  df <- tibble(g = c(1, 1, 2, 2), x = c(1, 2, 3, 4))
  df <- group_by(df, g)

  # Constant
  out <- mutate(df, .when = x > 5, y = 1L)
  expect_identical(out$y, rep(NA_integer_, 4))

  # Column
  out <- mutate(df, .when = x > 5, y = x)
  expect_identical(out$y, rep(NA_real_, 4))

  # Expression
  out <- mutate(df, .when = x > 5, y = x + 1)
  expect_identical(out$y, rep(NA_real_, 4))
})

test_that("rowwise - applies expression even when `.when` excludes all rows", {
  df <- tibble(x = c(1, 2, 3, 4), y = list(1, 2, 3, 4))
  df <- rowwise(df)

  # Constant
  out <- mutate(df, .when = x > 5, z = 1L)
  expect_identical(out$z, rep(NA_integer_, 4))

  # Column
  # TODO: Not the right result, see:
  # https://github.com/tidyverse/dplyr/issues/6302
  out <- mutate(df, .when = x > 5, z = y)
  expect_identical(out$z, rep(list(NULL), 4))
  # expect_identical(out$z, rep(NA_real_, 4))

  # Expression
  # TODO: Errors because `y` is accessed as the full list-col, so it does
  # `list() + 1`.
  # https://github.com/tidyverse/dplyr/issues/6303
  # out <- mutate(df, .when = x > 5, z = y + 1)
  # expect_identical(out$z, rep(NA_real_, 4))
})

test_that("grouped - applies expression on empty grouped data frame", {
  df <- tibble(g = integer(), x = double())
  df <- group_by(df, g)

  # Constant
  out <- mutate(df, .when = x > 2, y = 1L)
  expect_identical(out$y, integer())

  # Column
  out <- mutate(df, .when = x > 2, y = x)
  expect_identical(out$y, double())

  # Expression
  out <- mutate(df, .when = x > 2, y = x + 1)
  expect_identical(out$y, double())
})

test_that("across() works when using `.when`", {
  df <- tibble(x = c(1, 2, 3, 4), y = c(5, 6, 7, 8))

  out <- mutate(df, .when = x > 2, across(x:y, mean))

  expect_identical(out$x, c(1, 2, 3.5, 3.5))
  expect_identical(out$y, c(5, 6, 7.5, 7.5))
})

test_that("grouped - across() works when using `.when`", {
  df <- tibble(
    x = c(1, 2, 3, 4, 5, 7),
    y = c(5, 6, 7, 8, 9, 11),
    g = c(1, 1, 2, 3, 3, 2)
  )
  df <- group_by(df, g)

  out <- mutate(df, .when = x > 2, across(x:y, mean))

  expect_identical(out$x, c(1, 2, 5, 4.5, 4.5, 5))
  expect_identical(out$y, c(5, 6, 9, 8.5, 8.5, 9))
})

test_that("grouped - groups are ignored when computing `.when`", {
  df <- tibble(x = c(1, 2, 3, 4), g = c(1, 1, 2, 2))
  df <- group_by(df, g)

  # Global mean, not per group mean. Matches execution timing of SQL's `WHERE`.
  out <- mutate(df, .when = x >= mean(x), flag = TRUE)

  expect_identical(out$flag, c(NA, NA, TRUE, TRUE))
})

test_that("rowwise - `.when` is not evaluated rowwise", {
  # Because `.when` isn't evaluated per-group either.
  # `.when` ignores all grouping.

  df <- tibble(x = c(1, 2, 3, 4), y = list(1, 2, 3, "x"))
  df <- rowwise(df)

  out <- mutate(df, .when = map_lgl(y, is.double), z = y + 1)

  expect_identical(out$z, c(2, 3, 4, NA))
})

test_that("`n()` is applied to filtered data", {
  df <- tibble(x = c(1, 2, 3, 1, 2, 3, 4))

  out <- mutate(df, .when = x > 2, y = n())

  expect_identical(out$y, c(NA, NA, 3L, NA, NA, 3L, 3L))
})

test_that("grouped - `n()` is applied to filtered data", {
  df <- tibble(g = c(1, 1, 1, 2, 2, 2, 2), x = c(1, 2, 3, 1, 2, 3, 4))
  df <- group_by(df, g)

  out <- mutate(df, .when = x > 2, y = n())

  expect_identical(out$y, c(NA, NA, 1L, NA, NA, 2L, 2L))
})

test_that("rowwise - `n()` is applied to filtered data", {
  df <- tibble(x = c(1, 2, 3, 1, 2, 3, 4))
  df <- rowwise(df)

  out <- mutate(df, .when = x > 2, y = n())

  expect_identical(out$y, c(NA, NA, 1L, NA, NA, 1L, 1L))
})

test_that("`cur_data*()` is applied to filtered data", {
  df <- tibble(x = c(3, 1, 2, 3, 4))

  out <- mutate(df, .when = x > 2, y = cur_data())
  expect_identical(out$y, df[c(1, NA, NA, 4, 5),])

  out <- mutate(df, .when = x > 2, y = cur_data_all())
  expect_identical(out$y, df[c(1, NA, NA, 4, 5),])
})

test_that("grouped - `cur_data*()` is applied to filtered data", {
  df <- tibble(x = c(3, 1, 2, 3, 4), g = c(1, 1, 2, 2, 2))
  df <- group_by(df, g)

  out <- mutate(df, .when = x > 2, y = cur_data())
  expect_identical(out$y, df[c(1, NA, NA, 4, 5), "x"])

  out <- mutate(df, .when = x > 2, y = cur_data_all())
  expect_identical(out$y, ungroup(df[c(1, NA, NA, 4, 5),]))
})

test_that("rowwise - `cur_data*()` is applied to filtered data", {
  df <- tibble(x = c(3, 1, 2, 3, 4), y = list(1, 2, 3, 4, 5))
  df <- rowwise(df)

  out <- mutate(df, .when = x > 2, z = cur_data())
  expect_identical(out$z, as_tibble(df[c(1, NA, NA, 4, 5),]))

  out <- mutate(df, .when = x > 2, z = cur_data_all())
  expect_identical(out$z, as_tibble(df[c(1, NA, NA, 4, 5),]))
})

test_that("`cur_group*()` is applied to filtered data", {
  df <- tibble(x = c(3, 1, 2, 3, 4))

  out <- mutate(df, .when = x > 2, y = cur_group())
  expect_identical(out$y, new_tibble(list(), nrow = 5))

  out <- mutate(df, .when = x > 2, y = cur_group_id())
  expect_identical(out$y, c(1L, NA, NA, 1L, 1L))

  out <- mutate(df, .when = x > 2, y = cur_group_rows())
  expect_identical(out$y, c(1L, NA, NA, 4L, 5L))

  # Filtered out all data
  out <- mutate(df, .when = x > 5, y = cur_group())
  expect_identical(out$y, new_tibble(list(), nrow = 5))

  out <- mutate(df, .when = x > 5, y = cur_group_id())
  expect_identical(out$y, as.integer(c(NA, NA, NA, NA, NA)))

  out <- mutate(df, .when = x > 5, y = cur_group_rows())
  expect_identical(out$y, as.integer(c(NA, NA, NA, NA, NA)))
})

test_that("grouped - `cur_group*()` is applied to filtered data", {
  df <- tibble(g = c(1, 1, 2, 2), x = c(1, 2, 3, 4))
  df <- group_by(df, g)

  out <- mutate(df, .when = x > 1, y = cur_group())
  expect_identical(out$y, as_tibble(df[c(NA, 2, 3, 4), "g"]))

  out <- mutate(df, .when = x > 1, y = cur_group_id())
  expect_identical(out$y, c(NA, 1L, 2L, 2L))

  out <- mutate(df, .when = x > 1, y = cur_group_rows())
  expect_identical(out$y, c(NA, 2L, 3L, 4L))

  # Completely filtered out `g == 1`.
  # Keys should be updated so that the first key corresponds to `g == 2`.
  out <- mutate(df, .when = x > 2, y = cur_group())
  expect_identical(out$y, as_tibble(df[c(NA, NA, 3, 4), "g"]))

  out <- mutate(df, .when = x > 2, y = cur_group_id())
  expect_identical(out$y, c(NA, NA, 1L, 1L))

  out <- mutate(df, .when = x > 2, y = cur_group_rows())
  expect_identical(out$y, c(NA, NA, 3L, 4L))
})

test_that("rowwise - `cur_group*()` is applied to filtered data", {
  df <- tibble(x = c(1, 2, 3, 4), y = list(5, 6, 7, 8))
  df <- rowwise(df)

  out <- mutate(df, .when = x > 1, z = cur_group())
  expect_identical(out$z, new_tibble(list(), nrow = 4))

  out <- mutate(df, .when = x > 1, z = cur_group_id())
  expect_identical(out$z, c(NA, 1L, 2L, 3L))

  out <- mutate(df, .when = x > 1, z = cur_group_rows())
  expect_identical(out$z, c(NA, 2L, 3L, 4L))
})

test_that("can't remove columns when using `.when`", {
  df <- tibble(x = c(1, 2))

  expect_snapshot(error = TRUE, {
    mutate(df, .when = x > 1, x = NULL)
  })
  expect_snapshot(error = TRUE, {
    mutate(df, .when = x > 1, y = NULL)
  })
})

test_that("grouped - can't remove columns when using `.when`", {
  df <- tibble(x = c(2, 2, 1), g = c(1, 2, 2))
  df <- group_by(df, g)

  expect_snapshot(error = TRUE, {
    mutate(df, .when = x > 1, x = NULL)
  })
  expect_snapshot(error = TRUE, {
    mutate(df, .when = x > 1, y = NULL)
  })
})

test_that("rowwise - can't remove columns when using `.when`", {
  df <- tibble(x = c(2, 2, 1))
  df <- rowwise(df)

  expect_snapshot(error = TRUE, {
    mutate(df, .when = x > 1, x = NULL)
  })
  expect_snapshot(error = TRUE, {
    mutate(df, .when = x > 1, y = NULL)
  })
})

test_that("can have an unnamed `NULL` when using `.when`", {
  df <- tibble(x = c(1, 2))

  expect_identical(mutate(df, .when = x > 1, NULL), df)
})

test_that("original columns in `.data` are type stable when using `.when`", {
  df <- tibble(x = c(1L, 2L))

  expect_snapshot(error = TRUE, {
    mutate(df, .when = x > 1L, x = 1.5)
  })
  expect_snapshot(error = TRUE, {
    mutate(df, .when = x > 1L, x = x + 1.5)
  })
})

test_that("grouped - original columns in `.data` are type stable when using `.when`", {
  df <- tibble(x = c(2L, 2L, 1L), g = c(1, 2, 2))
  df <- group_by(df, g)

  expect_snapshot(error = TRUE, {
    mutate(df, .when = x > 1L, x = 1.5)
  })
  expect_snapshot(error = TRUE, {
    mutate(df, .when = x > 1L, x = x + 1.5)
  })
})

test_that("rowwise - original columns in `.data` are type stable when using `.when`", {
  df <- tibble(x = c(2L, 2L, 1L))
  df <- rowwise(df)

  expect_snapshot(error = TRUE, {
    mutate(df, .when = x > 1L, x = 1.5)
  })
  expect_snapshot(error = TRUE, {
    mutate(df, .when = x > 1L, x = x + 1.5)
  })
})

test_that("`.when` can generate new columns", {
  df <- tibble(x = c(1, 2, 2, 1))

  out <- mutate(df, .when = x > 1, y = 3)

  expect_identical(out$y, c(NA, 3, 3, NA))
})

test_that("generated columns can change types before being finalized", {
  df <- tibble(x = c(2L, 2L, 1L))

  out <- mutate(df, .when = x > 1L, y = 3L, y = y + 1.5)

  expect_identical(out$y, c(4.5, 4.5, NA))
})

test_that("grouped - generated columns can change types before being finalized", {
  df <- tibble(x = c(2L, 2L, 1L), g = c(1, 2, 2))
  df <- group_by(df, g)

  out <- mutate(df, .when = x > 1L, y = 3L, y = y + 1.5)

  expect_identical(out$y, c(4.5, 4.5, NA))
})

test_that("`.when` errors give context even if they aren't dplyr errors", {
  df <- tibble(x = c(1, 2))

  expect_snapshot(error = TRUE, {
    mutate(df, .when = a)
  })
})

test_that("`.when` must evaluate to a logical vector", {
  df <- tibble(x = c(1, 2))

  expect_snapshot(error = TRUE, {
    mutate(df, .when = x)
  })
})

test_that("grouped - `.when` must evaluate to a logical vector", {
  df <- tibble(x = c(1, 2), g = c(1, 2))
  df <- group_by(df, g)

  expect_snapshot(error = TRUE, {
    mutate(df, .when = x)
  })
})

test_that("`.when` must evaluate to a vector of the same size as `.data`", {
  df <- tibble(x = c(1, 2))

  expect_snapshot(error = TRUE, {
    mutate(df, .when = TRUE)
  })
})

test_that("grouped - `.when` must evaluate to a vector of the same size as `.data`", {
  df <- tibble(x = c(1, 2), g = c(1, 2))
  df <- group_by(df, g)

  expect_snapshot(error = TRUE, {
    mutate(df, .when = TRUE)
  })
})

test_that("`.when` can be used with `.before` / `.after`", {
  df <- tibble(x = c(2L, 2L, 1L), y = c(1, 2, 3))

  out <- mutate(df, .when = x > 1L, z = 3, .before = x)
  expect_identical(colnames(out), c("z", "x", "y"))

  out <- mutate(df, .when = x > 1L, z = 3, .after = x)
  expect_identical(colnames(out), c("x", "z", "y"))
})

test_that("`.when` can be used with `.keep` - `.when` columns aren't marked as used", {
  df <- tibble(x = c(2L, 2L, 1L), y = c(1, 2, 3))

  out <- mutate(df, .when = x > 1L, y = 5, .keep = "used")
  expect_identical(colnames(out), "y")
  expect_identical(out$y, c(5, 5, 3))
})
