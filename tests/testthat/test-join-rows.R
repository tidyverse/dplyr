test_that("`relationship` default behavior is correct", {
  # "warn-many-to-many" for equality joins
  expect_snapshot(out <- join_rows(c(1, 1), c(1, 1), condition = "=="))
  expect_equal(out$x, c(1L, 1L, 2L, 2L))
  expect_equal(out$y, c(1L, 2L, 1L, 2L))

  # "none" for rolling joins
  expect_warning(out <- join_rows(c(1, 2), c(1, 1), condition = ">=", filter = "max"), NA)
  expect_equal(out$x, c(1L, 1L, 2L, 2L))
  expect_equal(out$y, c(1L, 2L, 1L, 2L))
  # If rolling joins warned on many-to-many relationships, it would be a little
  # hard to explain that the above example warns, but this wouldn't just because
  # we've removed `2` as a key from `x`:
  # `join_rows(1, c(1, 1), condition = ">=", filter = "max")`

  # "none" for inequality joins (and overlap joins)
  expect_warning(out <- join_rows(c(1, 2), c(0, 1), condition = ">="), NA)
  expect_equal(out$x, c(1L, 1L, 2L, 2L))
  expect_equal(out$y, c(1L, 2L, 1L, 2L))

  # "none" for deprecated cross joins
  expect_warning(out <- join_rows(c(1, 1), c(1, 1), cross = TRUE), NA)
  expect_equal(out$x, c(1L, 1L, 2L, 2L))
  expect_equal(out$y, c(1L, 2L, 1L, 2L))
})

test_that("`multiple` first/last/any works correctly", {
  out <- join_rows(c(1, 1), c(1, 1), multiple = "first")
  expect_equal(out$x, c(1L, 2L))
  expect_equal(out$y, c(1L, 1L))

  out <- join_rows(c(1, 1), c(1, 1), multiple = "last")
  expect_equal(out$x, c(1L, 2L))
  expect_equal(out$y, c(2L, 2L))

  out <- join_rows(c(1, 1), c(1, 1), multiple = "any")
  expect_equal(out$x, c(1L, 2L))
  expect_equal(out$y %in% c(1L, 2L), c(TRUE, TRUE))
})

test_that("inner join only outputs matching keys", {
  out <- join_rows(c(2, 1), c(3, 4, 1), type = "inner")
  expect_equal(out$x, 2L)
  expect_equal(out$y, 3L)

  out <- join_rows(c(2, 1), c(3, 4, 1), type = "inner", condition = ">")
  expect_equal(out$x, 1L)
  expect_equal(out$y, 3L)
})

test_that("left join contains all keys from x", {
  out <- join_rows(c(2, 1), c(3, 4, 1), type = "left")
  expect_equal(out$x, c(1L, 2L))
  expect_equal(out$y, c(NA, 3L))

  out <- join_rows(c(2, 1), c(3, 4, 1), type = "left", condition = ">")
  expect_equal(out$x, c(1L, 2L))
  expect_equal(out$y, c(3L, NA))
})

test_that("right join contains all keys from y", {
  out <- join_rows(c(2, 1), c(3, 4, 1), type = "right")
  expect_equal(out$x, c(2L, NA, NA))
  expect_equal(out$y, c(3L, 1L, 2L))

  out <- join_rows(c(2, 1), c(3, 4, 1), type = "right", condition = ">=")
  expect_equal(out$x, c(1L, 2L, NA, NA))
  expect_equal(out$y, c(3L, 3L, 1L, 2L))
})

test_that("full join contains all keys from both", {
  out <- join_rows(c(2, 1), c(3, 1), type = "full")
  expect_equal(out$x, c(1L, 2L, NA))
  expect_equal(out$y, c(NA, 2L, 1L))

  out <- join_rows(c(2, 1), c(3, 1), type = "full", condition = ">")
  expect_equal(out$x, c(1L, 2L, NA))
  expect_equal(out$y, c(2L, NA, 1L))
})

test_that("nest join returns 0L for unmatched x keys", {
  out <- join_rows(c(2, 1), c(3, 4, 1), type = "nest")
  expect_equal(out$x, c(1L, 2L))
  expect_equal(out$y, c(0L, 3L))
})

test_that("nest join returns 0L for missing x keys with `na_matches = 'never'`", {
  out <- join_rows(c(NA, 1), 1, type = "nest", na_matches = "never")
  expect_equal(out$x, c(1L, 2L))
  expect_equal(out$y, c(0L, 1L))
})

test_that("matching rows can be filtered", {
  out <- join_rows(c(3, 5), c(2, 4, 1), condition = ">=", filter = "max")
  expect_equal(out$x, 1:2)
  expect_equal(out$y, 1:2)

  out <- join_rows(c(3, 5), c(2, 4, 1), condition = ">=", filter = "min")
  expect_equal(out$x, 1:2)
  expect_equal(out$y, c(3, 3))
})

test_that("missing values only match with `==`, `>=`, and `<=` conditions", {
  out <- join_rows(NA, NA, condition = "==")
  expect_identical(out$x, 1L)
  expect_identical(out$y, 1L)

  out <- join_rows(NA, NA, condition = ">=")
  expect_identical(out$x, 1L)
  expect_identical(out$y, 1L)

  out <- join_rows(NA, NA, condition = "<=")
  expect_identical(out$x, 1L)
  expect_identical(out$y, 1L)

  out <- join_rows(NA, NA, condition = ">")
  expect_identical(out$x, integer())
  expect_identical(out$y, integer())

  out <- join_rows(NA, NA, condition = "<")
  expect_identical(out$x, integer())
  expect_identical(out$y, integer())


  x <- tibble(x = c(1, 1), y = c(2, NA))
  y <- tibble(x = c(1, 1), y = c(3, NA))

  out <- join_rows(x, y, condition = c("==", "<="))
  expect_identical(out$x, c(1L, 2L))
  expect_identical(out$y, c(1L, 2L))

  out <- join_rows(x, y, condition = c("==", "<"))
  expect_identical(out$x, 1L)
  expect_identical(out$y, 1L)
})

test_that("join_rows() doesn't error on unmatched rows if they won't be dropped", {
  # 2 is unmatched, but a left join means we always retain that key
  out <- join_rows(c(1, 2), 1, type = "left", unmatched = "error")
  expect_identical(out$x, c(1L, 2L))
  expect_identical(out$y, c(1L, NA))

  out <- join_rows(c(1, 2), c(1, 3), type = "full", unmatched = "error")
  expect_identical(out$x, c(1L, 2L, NA))
  expect_identical(out$y, c(1L, NA, 2L))
})

test_that("join_rows() allows `unmatched` to be specified independently for inner joins", {
  out <- join_rows(c(1, 2), 1, type = "inner", unmatched = c("drop", "error"))
  expect_identical(out$x, 1L)
  expect_identical(out$y, 1L)

  out <- join_rows(1, c(2, 1), type = "inner", unmatched = c("error", "drop"))
  expect_identical(out$x, 1L)
  expect_identical(out$y, 2L)

  # Both have dropped rows, only `y` is mentioned in the error
  expect_snapshot(error = TRUE, {
    join_rows(c(1, 3), c(1, 2), type = "inner", unmatched = c("drop", "error"))
  })
})

test_that("join_rows() expects incompatible type errors to have been handled by join_cast_common()", {
  expect_snapshot({
    (expect_error(
      join_rows(data.frame(x = 1), data.frame(x = factor("a")))
    ))
  })
})

test_that("join_rows() gives meaningful one-to-one errors", {
  expect_snapshot(error = TRUE, {
    join_rows(1, c(1, 1), relationship = "one-to-one")
  })
  expect_snapshot(error = TRUE, {
    join_rows(c(1, 1), 1, relationship = "one-to-one")
  })
})

test_that("join_rows() gives meaningful one-to-many errors", {
  expect_snapshot(error = TRUE, {
    join_rows(c(1, 1), 1, relationship = "one-to-many")
  })
})

test_that("join_rows() gives meaningful many-to-one errors", {
  expect_snapshot(error = TRUE, {
    join_rows(1, c(1, 1), relationship = "many-to-one")
  })
})

test_that("join_rows() gives meaningful many-to-many warnings", {
  expect_snapshot({
    join_rows(c(1, 1), c(1, 1))
  })

  # With proof that the defaults flow through user facing functions
  df <- data.frame(x = c(1, 1))
  expect_snapshot({
    left_join(df, df, by = join_by(x))
  })
})

test_that("join_rows() gives meaningful error message on unmatched rows", {
  # Unmatched in the RHS
  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = c(1, 2)),
      data.frame(x = c(3, 1)),
      type = "left",
      unmatched = "error"
    )
  )
  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = c(1, 2)),
      data.frame(x = c(3, 1)),
      type = "nest",
      unmatched = "error"
    )
  )

  # Unmatched in the LHS
  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = c(1, 2)),
      data.frame(x = c(3, 1)),
      type = "right",
      unmatched = "error"
    )
  )

  # Unmatched in either side
  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = c(1, 2)),
      data.frame(x = 1),
      type = "inner",
      unmatched = "error"
    )
  )
  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = c(1, 2)),
      data.frame(x = 1),
      type = "inner",
      unmatched = c("error", "drop")
    )
  )
  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = 1),
      data.frame(x = c(1, 2)),
      type = "inner",
      unmatched = "error"
    )
  )
  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = 1),
      data.frame(x = c(1, 2)),
      type = "inner",
      unmatched = c("drop", "error")
    )
  )
})

test_that("join_rows() always errors on unmatched missing values", {
  # Unmatched in the RHS
  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = 1),
      data.frame(x = NA),
      type = "left",
      unmatched = "error",
      na_matches = "na"
    )
  )
  expect_snapshot(
    error = TRUE,
    join_rows(
      data.frame(x = NA),
      data.frame(x = NA),
      type = "left",
      unmatched = "error",
      na_matches = "never"
    )
  )
  expect_snapshot(
    error = TRUE,
    join_rows(
      data.frame(x = 1),
      data.frame(x = NA),
      type = "nest",
      unmatched = "error",
      na_matches = "na"
    )
  )
  expect_snapshot(
    error = TRUE,
    join_rows(
      data.frame(x = NA),
      data.frame(x = NA),
      type = "nest",
      unmatched = "error",
      na_matches = "never"
    )
  )

  # Unmatched in the LHS
  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = NA),
      data.frame(x = 1),
      type = "right",
      unmatched = "error",
      na_matches = "na"
    )
  )
  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = NA),
      data.frame(x = NA),
      type = "right",
      unmatched = "error",
      na_matches = "never"
    )
  )

  # Unmatched in either side
  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = 1),
      data.frame(x = c(1, NA)),
      type = "inner",
      unmatched = "error",
      na_matches = "na"
    )
  )
  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = 1),
      data.frame(x = c(1, NA)),
      type = "inner",
      unmatched = c("drop", "error"),
      na_matches = "na"
    )
  )
  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = c(1, NA)),
      data.frame(x = 1),
      type = "inner",
      unmatched = "error",
      na_matches = "na"
    )
  )
  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = c(1, NA)),
      data.frame(x = 1),
      type = "inner",
      unmatched = c("error", "drop"),
      na_matches = "na"
    )
  )
  expect_snapshot(error = TRUE,
    join_rows(
      data.frame(x = NA),
      data.frame(x = NA),
      type = "inner",
      unmatched = "error",
      na_matches = "never"
    )
  )
})

test_that("join_rows() validates `unmatched`", {
  df <- tibble(x = 1)

  expect_snapshot(error = TRUE, {
    join_rows(df, df, unmatched = 1)
    join_rows(df, df, unmatched = "foo")

    # One `unmatched` input is allowed for most joins
    join_rows(df, df, type = "left", unmatched = character())
    join_rows(df, df, type = "left", unmatched = c("drop", "error"))

    # Two `unmatched` inputs are allowed for inner joins
    join_rows(df, df, type = "inner", unmatched = character())
    join_rows(df, df, type = "inner", unmatched = c("drop", "error", "error"))

    join_rows(df, df, type = "inner", unmatched = c("drop", "dr"))
  })
})

test_that("join_rows() validates `relationship`", {
  df <- tibble(x = 1)

  expect_snapshot(error = TRUE, {
    join_rows(df, df, relationship = 1)
  })

  # Notably can't use the vctrs options
  expect_snapshot(error = TRUE, {
    join_rows(df, df, relationship = "none")
  })
  expect_snapshot(error = TRUE, {
    join_rows(df, df, relationship = "warn-many-to-many")
  })
})

test_that("join_rows() rethrows overflow error nicely (#6912)", {
  skip_on_cran()
  # Windows 32-bit doesn't support long vectors of this size, and the
  # intermediate `r_ssize` will be too large
  skip_if(.Machine$sizeof.pointer < 8L, message = "No long vector support")

  df <- tibble(x = 1:1e7)

  expect_snapshot(error = TRUE, {
    join_rows(df, df, condition = ">=")
  })
})

# Deprecated behavior ----------------------------------------------------------

test_that("`multiple = NULL` is deprecated and results in `'all'` (#6731)", {
  df1 <- tibble(x = c(1, 2))
  df2 <- tibble(x = c(2, 1, 2))

  expect_snapshot({
    out <- join_rows(df1, df2, multiple = NULL)
  })
  expect_identical(out$x, c(1L, 2L, 2L))
  expect_identical(out$y, c(2L, 1L, 3L))

  expect_snapshot({
    left_join(df1, df2, by = join_by(x), multiple = NULL)
  })
})

test_that("`multiple = 'error'` is deprecated (#6731)", {
  df1 <- tibble(x = c(1, 2))
  df2 <- tibble(x = c(2, 1, 2))

  expect_snapshot(error = TRUE, {
    join_rows(df1, df2, multiple = "error")
  })
  expect_snapshot(error = TRUE, {
    left_join(df1, df2, by = join_by(x), multiple = "error")
  })
})

test_that("`multiple = 'warning'` is deprecated (#6731)", {
  df1 <- tibble(x = c(1, 2))
  df2 <- tibble(x = c(2, 1, 2))

  expect_snapshot({
    out <- join_rows(df1, df2, multiple = "warning")
  })
  expect_identical(out$x, c(1L, 2L, 2L))
  expect_identical(out$y, c(2L, 1L, 3L))

  expect_snapshot({
    left_join(df1, df2, by = join_by(x), multiple = "warning")
  })
})
