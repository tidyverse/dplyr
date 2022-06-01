# ------------------------------------------------------------------------------
# mutate_when()

test_that("can be used to update a column based on condition in another column", {
  df <- tibble(x = c(1, NA, NA), y = c(2, 3, 4))

  expect_identical(
    mutate_when(df, is.na(x), y = NA),
    tibble(x = c(1, NA, NA), y = c(2, NA, NA))
  )
})

test_that("can be used to update multiple named columns", {
  df <- tibble(x = c(1, NA, NA), y = c(2, 3, 4))

  expect_identical(
    mutate_when(df, is.na(x), y = 6, x = 5),
    tibble(x = c(1, 5, 5), y = c(2, 6, 6))
  )
})

test_that("first `TRUE` value overrides any conditions that come after it", {
  df <- tibble(x = c(1, 2, 3))

  out <- mutate_when(
    df,
    x <= 2, y = 4,
    x <= 3, z = 5
  )

  expect_identical(out$y, c(4, 4, NA))
  expect_identical(out$z, c(NA, NA, 5))
})

test_that("`NA` values in logical condition are treated as `FALSE`", {
  df <- tibble(x = c(1, NA, 2))

  out <- mutate_when(df, x > 1, y = 2, x = 0)

  expect_identical(out$x, c(1, NA, 0))
  expect_identical(out$y, c(NA, NA, 2))
})

test_that("value dots are vectorized and sliced appropriately", {
  df <- tibble(x = c(1, 2, 3), y = c(2, 3, 4))

  out <- mutate_when(df, x >= 2, x = x + y)

  expect_identical(out$x, c(1, 5, 7))
})

test_that("updates are evaluated only on the slice of `.data` where the condition is true", {
  df <- tibble(x = c(1, 2, NA))

  expect_identical(
    mutate_when(df, !is.na(x), y = mean(x)),
    tibble(x = c(1, 2, NA), y = c(1.5, 1.5, NA))
  )
})

test_that("condition can be directly followed by another condition", {
  df <- tibble(x = c(1, 2))

  expect_identical(
    mutate_when(
      df,
      x == 1,
      x < 3, y = 1
    ),
    tibble(x = c(1, 2), y = c(NA, 1))
  )
})

test_that("all conditions are evaluated on original data", {
  df <- tibble(x = 1)

  expect_identical(
    mutate_when(
      df,
      x == 1, x = 2,
      x == 2, x = 3
    ),
    tibble(x = 2)
  )
})

test_that("works with no `...`", {
  df <- tibble(x = 1, y = 2)
  expect_identical(mutate_when(df), df)
})

test_that("`mutate_when()` - is type stable on input", {
  df <- tibble(x = 1L)

  expect_identical(
    mutate_when(df, x == 1L, x = 2),
    tibble(x = 2L)
  )

  expect_snapshot(error = TRUE, {
    mutate_when(df, x == 1L, x = "x")
  })
})

test_that("`mutate_when()` - is size stable on input", {
  df <- tibble(x = 1:3)

  expect_snapshot(error = TRUE, {
    mutate_when(df, x >= 2, x = 5:7)
  })
})

test_that("conditions must be logical vectors", {
  df <- tibble(x = c(1, 2))

  expect_snapshot(error = TRUE, {
    mutate_when(df, x + 1, x = x + 2)
  })

  # TODO: Suboptimal indexing. Should say `..3`
  expect_snapshot(error = TRUE, {
    mutate_when(
      df,
      x == 1, x = x + 2,
      x + 99, x = x + 2
    )
  })
})

test_that("`mutate_when()` - enforce that columns can't be removed", {
  df <- tibble(x = c(1, 2), y = c(3, 4))

  expect_snapshot(error = TRUE, {
    mutate_when(df, x > 5, y = NULL)
  })
  expect_snapshot(error = TRUE, {
    mutate_when(df, x > 5, x = 1, y < 5, x = 2, y = NULL)
  })
})

test_that("`...` must start with an unnamed input", {
  df <- tibble(x = 1)

  expect_snapshot(error = TRUE, {
    mutate_when(df, x = x)
  })
})

# ------------------------------------------------------------------------------
# revise()

test_that("can be used to update a column based on condition in another column", {
  df <- tibble(x = c(1, NA, NA), y = c(2, 3, 4))

  expect_identical(
    revise(df, is.na(x), y = NA),
    tibble(x = c(1, NA, NA), y = c(2, NA, NA))
  )
})

test_that("can be used to update multiple named columns", {
  df <- tibble(x = c(1, NA, NA), y = c(2, 3, 4))

  expect_identical(
    mutate_when(df, is.na(x), y = 6, x = 5),
    tibble(x = c(1, 5, 5), y = c(2, 6, 6))
  )
})

test_that("`n()` is evaluated on the slice of data", {
  df <- tibble(x = c(1, NA, NA))

  expect_identical(
    revise(df, is.na(x), y = seq_len(n())),
    tibble(x = c(1, NA, NA), y = c(NA, 1L, 2L))
  )
})

test_that("expressions are evaluated only on the slice of `.data` where the condition is true", {
  df <- tibble(x = c(1, 2, NA))

  expect_identical(
    revise(df, !is.na(x), y = mean(x)),
    tibble(x = c(1, 2, NA), y = c(1.5, 1.5, NA))
  )
})

test_that("can technically insert a constant that happens to be the right size", {
  # This is a side effect of evaluating `...` only on a slice of `.data`.
  # It isn't ideal, but there is no way to stop it, and we really do want
  # to evaluate `...` on the slice in most other cases.
  df <- tibble(x = c(1, 2, NA, NA, NA))

  out <- revise(df, is.na(x), x = c(5, 6, 7))

  expect_identical(out$x, c(1, 2, 5, 6, 7))
})

test_that("`.when` is computed within groups", {
  df <- tibble(
    g = c(1, 1, 1, 2, 2, 2),
    x = 1:6
  )
  gdf <- group_by(df, g)

  out <- revise(gdf, x > mean(x), y = 1)

  expect_identical(out$y, c(NA, NA, 1, NA, NA, 1))
})

test_that("`...` are computed within groups", {
  df <- tibble(
    g = c(1, 1, 1, 2, 2, 2),
    x = c(1, NA, 3, 1, NA, NA)
  )
  gdf <- group_by(df, g)

  out <- revise(gdf, is.na(x), x = length(x), y = n())

  expect_identical(out$x, c(1, 1, 3, 1, 2, 2))
  expect_identical(out$y, c(NA, 1L, NA, NA, 2L, 2L))
})

test_that("can use `cur_data_*()`", {
  df <- tibble(
    g = c(1, 1, 1, 2, 2, 2),
    x = c(1, NA, 3, 1, NA, NA),
    y = 1:6
  )
  gdf <- group_by(df, g)

  out <- revise(gdf, is.na(x), z = cur_data())
  expect_identical(out$z, df[c(NA, 2, NA, NA, 5, 6), c("x", "y")])

  out <- revise(gdf, is.na(x), z = cur_data_all())
  expect_identical(out$z, df[c(NA, 2, NA, NA, 5, 6), c("g", "x", "y")])
})

test_that("can use `cur_group_*()", {
  df <- tibble(
    g = c(1, 1, 1, 3, 3, 3),
    x = c(1, NA, 3, 1, NA, NA),
    y = 1:6
  )
  gdf <- group_by(df, g)

  out <- revise(gdf, is.na(x), z = cur_group())
  expect_identical(out$z, tibble(g = c(NA, 1, NA, NA, 3, 3)))

  out <- revise(gdf, is.na(x), z = cur_group_id())
  expect_identical(out$z, c(NA, 1L, NA, NA, 2L, 2L))

  out <- revise(gdf, is.na(x), z = cur_group_rows())
  expect_identical(out$z, c(NA, 1L, NA, NA, 2L, 3L))
})

test_that("`NA` values in logical condition are treated as `FALSE`", {
  df <- tibble(x = c(1, NA, 2))

  out <- revise(df, x > 1, y = 2, x = 0)

  expect_identical(out$x, c(1, NA, 0))
  expect_identical(out$y, c(NA, NA, 2))
})

test_that("value dots are vectorized and sliced appropriately", {
  df <- tibble(x = c(1, 2, 3), y = c(2, 3, 4))

  out <- revise(df, x >= 2, x = x + y)

  expect_identical(out$x, c(1, 5, 7))
})

test_that("works with no `...`", {
  df <- tibble(x = 1, y = 2)
  expect_identical(revise(df, x >= 1), df)
})

test_that("`revise()` - is type stable on input", {
  df <- tibble(x = 1L)

  expect_identical(
    revise(df, x == 1L, x = 2),
    tibble(x = 2L)
  )

  expect_snapshot(error = TRUE, {
    revise(df, x == 1L, x = "x")
  })
})

test_that("`revise()` - is size stable on input", {
  df <- tibble(x = 1:3)

  expect_snapshot(error = TRUE, {
    revise(df, x >= 2, x = 5:7)
  })
})

test_that("works with `across()`", {
  df <- tibble(x = 1:3, y = 4:6)

  out <- revise(df, x > 1, across(x:y, sum))

  expect_identical(out$x, c(1L, 5L, 5L))
  expect_identical(out$y, c(4L, 11L, 11L))
})

test_that("works with unnamed data frames", {
  df <- tibble(x = 1:3, y = 4:6)

  out <- revise(df, x > 1, tibble(y = 99, z = 0L))

  expect_identical(out$y, c(4L, 99L, 99L))
  expect_identical(out$z, c(NA, 0L, 0L))
})

test_that("`.when` must be a logical vector", {
  df <- tibble(x = c(1, 2))

  expect_snapshot(error = TRUE, {
    revise(df, x + 1, x = x + 2)
  })
})

test_that("`revise()` - enforce that columns can't be removed", {
  df <- tibble(x = c(1, 2), y = c(3, 4))

  expect_snapshot(error = TRUE, {
    revise(df, x > 5, y = NULL)
  })
  expect_snapshot(error = TRUE, {
    revise(df, x > 5, x = 1, y = NULL)
  })
})

test_that("unnamed `NULL`s are compacted", {
  df <- tibble(x = 1)
  expect_identical(revise(df, x >= 1, NULL), df)
})
