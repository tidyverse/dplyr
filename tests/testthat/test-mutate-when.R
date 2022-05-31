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

test_that("is type stable on input", {
  df <- tibble(x = 1L)

  expect_identical(
    mutate_when(df, x == 1L, x = 2),
    tibble(x = 2L)
  )

  expect_snapshot(error = TRUE, {
    mutate_when(df, x == 1L, x = "x")
  })
})

test_that("is size stable on input", {
  skip("Haven't figured this out yet")

  df <- tibble(x = 1:3)

  # TODO: This should probably fail? Maybe there is no way to do that.
  mutate_when(df, x >= 2, x = 5:6)
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

test_that("enforce that columns can't be removed", {
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
