# ------------------------------------------------------------------------------
# rows_insert()

test_that("rows_insert() works", {
  data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)

  expect_identical(
    rows_insert(data, tibble(a = 4L, b = "z"), by = "a"),
    tibble(a = 1:4, b = c("a", "b", NA, "z"), c = c(0.5, 1.5, 2.5, NA))
  )
})

test_that("rows_insert() doesn't allow insertion of matched keys by default", {
  x <- tibble(a = 1, b = 2)

  y <- tibble(a = 1, b = 3)

  expect_snapshot(
    (expect_error(rows_insert(x, y, by = "a")))
  )

  y <- tibble(a = c(1, 1, 1), b = c(3, 4, 5))

  expect_snapshot(
    (expect_error(rows_insert(x, y, by = "a")))
  )
})

test_that("rows_insert() allows you to ignore matched keys with `conflict = 'ignore'`", {
  x <- tibble(a = 1, b = 2)

  y <- tibble(a = 1, b = 3)

  expect_identical(rows_insert(x, y, by = "a", conflict = "ignore"), x)

  y <- tibble(a = c(1, 2, 1), b = c(3, 4, 5))

  expect_identical(
    rows_insert(x, y, by = "a", conflict = "ignore"),
    rows_insert(x, y[2,], by = "a")
  )
})

test_that("rows_insert() allows `x` keys to be duplicated (#5553)", {
  x <- tibble(a = c(1, 1), b = c(2, 3))
  y <- tibble(a = 2, b = 4)

  expect_identical(
    rows_insert(x, y, by = "a"),
    tibble(a = c(1, 1, 2), b = c(2, 3, 4))
  )
})

test_that("rows_insert() allows `y` keys to be duplicated (#5553)", {
  x <- tibble(a = 2, b = 4)
  y <- tibble(a = c(1, 1), b = c(2, 3))

  expect_identical(
    rows_insert(x, y, by = "a"),
    tibble(a = c(2, 1, 1), b = c(4, 2, 3))
  )
})

test_that("rows_insert() casts keys to the type of `x`", {
  x <- vctrs::data_frame(key = 1L, value = 2)
  y <- vctrs::data_frame(key = 1.5, value = 1.5)

  expect_snapshot({
    (expect_error(rows_insert(x, y, "key")))
  })
})

test_that("rows_insert() casts values to the type of `x`", {
  x <- vctrs::data_frame(key = 1, value = 2L)
  y <- vctrs::data_frame(key = 2, value = 1.5)

  expect_snapshot({
    (expect_error(rows_insert(x, y, "key")))
  })
})

test_that("`conflict` is validated", {
  x <- tibble(a = 1)
  y <- tibble(a = 2)

  expect_snapshot({
    (expect_error(rows_insert(x, y, by = "a", conflict = "foo")))
    (expect_error(rows_insert(x, y, by = "a", conflict = 1)))
  })
})

# ------------------------------------------------------------------------------
# rows_append()

test_that("rows_append() allows you to insert unconditionally", {
  x <- tibble(a = 1, b = 2)
  y <- tibble(a = 1, b = 3)

  expect_identical(rows_append(x, y), bind_rows(x, y))

  y <- tibble(a = c(1, 2, 1), b = c(3, 4, 5))

  expect_identical(rows_append(x, y), bind_rows(x, y))
})

test_that("rows_append() casts to the type of `x`", {
  x <- vctrs::data_frame(key = 1L, value = 2)

  y <- vctrs::data_frame(key = 1.5, value = 1.5)

  expect_snapshot({
    (expect_error(rows_append(x, y)))
  })

  y <- vctrs::data_frame(key = 2, value = 3L)

  out <- rows_append(x, y)
  expect_identical(out$key, c(1L, 2L))
  expect_identical(out$value, c(2, 3))
})

test_that("rows_append() requires that `y` columns be a subset of `x`", {
  x <- tibble(a = 1, b = 2)
  y <- tibble(a = 1, b = 2, c = 3)

  expect_snapshot({
    (expect_error(rows_append(x, y)))
  })
})

test_that("rows_append() doesn't require that `x` columns be a subset of `y`", {
  x <- tibble(a = 1, b = 2, c = 3)
  y <- tibble(a = 1, b = 2)

  out <- rows_append(x, y)
  expect_identical(out$c, c(3, NA))
})

# ------------------------------------------------------------------------------

test_that("rows_update() works", {
  data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)

  expect_identical(
    rows_update(data, tibble(a = 2:3, b = "z"), by = "a"),
    tibble(a = 1:3, b = c("a", "z", "z"), c = data$c)
  )

  expect_silent(
    expect_identical(
      rows_update(data, tibble(b = "z", a = 2:3), by = "a"),
      tibble(a = 1:3, b = c("a", "z", "z"), c = data$c)
    )
  )
})

test_that("rows_update() requires `y` keys to exist in `x` by default", {
  x <- tibble(a = 1, b = 2)
  y <- tibble(a = c(2, 1, 3), b = c(1, 1, 1))

  expect_snapshot((expect_error(rows_update(x, y, "a"))))
})

test_that("rows_update() allows `y` keys that don't exist in `x` to be ignored", {
  x <- tibble(a = 1, b = 2)
  y <- tibble(a = c(2, 1, 3), b = c(1, 1, 1))

  expect_identical(
    rows_update(x, y, "a", unmatched = "ignore"),
    tibble(a = 1, b = 1)
  )
})

test_that("rows_update() allows `x` keys to be duplicated (#5553)", {
  x <- tibble(a = c(1, 2, 1, 3), b = c(2, 3, 4, 5), c = letters[1:4])
  y <- tibble(a = c(1, 3), b = c(99, 88))

  expect_identical(
    rows_update(x, y, by = "a"),
    tibble(a = c(1, 2, 1, 3), b = c(99, 3, 99, 88), c = letters[1:4])
  )
})

test_that("rows_update() doesn't allow `y` keys to be duplicated (#5553)", {
  x <- tibble(a = 2, b = 4)
  y <- tibble(a = c(1, 1), b = c(2, 3))

  expect_snapshot((expect_error(rows_update(x, y, by = "a"))))
})

test_that("rows_update() avoids bare data.frame `drop = FALSE` issues", {
  x <- vctrs::data_frame(x = c(1, 2, 3), y = I(list(1:2, 3:4, 5:6)))
  y <- vctrs::data_frame(x = c(1, 3), y = I(list(0L, 100:101)))

  out <- rows_update(x, y, "x")
  expect_identical(out$y, I(list(0L, 3:4, 100:101)))
})

test_that("rows_update() casts keys to their common type for matching but retains `x` type", {
  x <- vctrs::data_frame(key = 1L, value = 2)
  y <- vctrs::data_frame(key = 1, value = 1.5)

  out <- rows_update(x, y, "key")
  expect_identical(out$key, x$key)
  expect_identical(out$value, y$value)

  y <- vctrs::data_frame(key = "x", value = 1.5)

  expect_snapshot({
    (expect_error(rows_update(x, y, "key")))
  })
})

test_that("rows_update() casts values to the type of `x`", {
  x <- vctrs::data_frame(key = 1, value = 2L)
  y <- vctrs::data_frame(key = 1, value = 1.5)

  expect_snapshot({
    (expect_error(rows_update(x, y, "key")))
  })

  out <- rows_update(y, x, "key")
  expect_identical(out$value, 2)
})

test_that("`unmatched` is validated", {
  x <- tibble(a = 1)
  y <- tibble(a = 1)

  expect_snapshot({
    (expect_error(rows_update(x, y, by = "a", unmatched = "foo")))
    (expect_error(rows_update(x, y, by = "a", unmatched = 1)))
  })
})

# ------------------------------------------------------------------------------

test_that("rows_patch() works", {
  data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)

  expect_identical(
    rows_patch(data, tibble(a = 2:3, b = "z"), by = "a"),
    tibble(a = 1:3, b = c("a", "b", "z"), c = data$c)
  )

  expect_silent(
    expect_identical(
      rows_patch(data, tibble(b = "z", a = 2:3), by = "a"),
      tibble(a = 1:3, b = c("a", "b", "z"), c = data$c)
    )
  )
})

test_that("rows_patch() requires `y` keys to exist in `x` by default", {
  x <- tibble(a = 1, b = 2)
  y <- tibble(a = c(2, 1, 3), b = c(1, 1, 1))

  expect_snapshot((expect_error(rows_patch(x, y, "a"))))
})

test_that("rows_patch() allows `y` keys that don't exist in `x` to be ignored", {
  x <- tibble(a = 1, b = NA_real_)
  y <- tibble(a = c(2, 1, 3), b = c(1, 1, 1))

  expect_identical(
    rows_patch(x, y, "a", unmatched = "ignore"),
    tibble(a = 1, b = 1)
  )
})

test_that("rows_patch() allows `x` keys to be duplicated (#5553)", {
  x <- tibble(a = c(1, 2, 1, 3), b = c(NA, 3, 4, NA), c = letters[1:4])
  y <- tibble(a = c(1, 3), b = c(99, 88))

  expect_identical(
    rows_patch(x, y, by = "a"),
    tibble(a = c(1, 2, 1, 3), b = c(99, 3, 4, 88), c = letters[1:4])
  )
})

test_that("rows_patch() doesn't allow `y` keys to be duplicated (#5553)", {
  x <- tibble(a = 2, b = 4)
  y <- tibble(a = c(1, 1), b = c(2, 3))

  expect_snapshot((expect_error(rows_patch(x, y, by = "a"))))
})

test_that("rows_patch() avoids bare data.frame `drop = FALSE` issues", {
  x <- vctrs::data_frame(x = c(1, 2, 3, 3), y = c(NA, 5, NA, 6))
  y <- vctrs::data_frame(x = c(1, 3), y = c(0, 100))

  out <- rows_patch(x, y, "x")
  expect_identical(out$y, c(0, 5, 100, 6))
})

test_that("rows_patch() casts keys to their common type for matching but retains `x` type", {
  x <- vctrs::data_frame(key = 1L, value = NA_real_)
  y <- vctrs::data_frame(key = 1, value = 1.5)

  out <- rows_patch(x, y, "key")
  expect_identical(out$key, x$key)
  expect_identical(out$value, y$value)

  y <- vctrs::data_frame(key = "x", value = 1.5)

  expect_snapshot({
    (expect_error(rows_patch(x, y, "key")))
  })
})

test_that("rows_patch() casts values to the type of `x`", {
  x <- vctrs::data_frame(key = 1, value = 2L)
  y <- vctrs::data_frame(key = 1, value = 1.5)

  expect_snapshot({
    (expect_error(rows_patch(x, y, "key")))
  })
})

# ------------------------------------------------------------------------------

test_that("rows_upsert() works", {
  data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)

  expect_identical(
    rows_upsert(data, tibble(a = 2:4, b = "z"), by = "a"),
    tibble(a = 1:4, b = c("a", "z", "z", "z"), c = c(data$c, NA))
  )
})

test_that("rows_upsert() allows `x` keys to be duplicated (#5553)", {
  x <- tibble(a = c(1, 2, 1, 3), b = c(NA, 3, 4, NA), c = letters[1:4])
  y <- tibble(a = c(1, 3, 4), b = c(99, 88, 100))

  expect_identical(
    rows_upsert(x, y, by = "a"),
    tibble(a = c(1, 2, 1, 3, 4), b = c(99, 3, 99, 88, 100), c = c(letters[1:4], NA))
  )
})

test_that("rows_upsert() doesn't allow `y` keys to be duplicated (#5553)", {
  x <- tibble(a = 2, b = 4)
  y <- tibble(a = c(1, 1), b = c(2, 3))

  expect_snapshot((expect_error(rows_upsert(x, y, by = "a"))))
})

test_that("rows_upsert() avoids bare data.frame `drop = FALSE` issues", {
  x <- vctrs::data_frame(x = c(1, 2, 3), y = I(list(1:2, 3:4, 5:6)))
  y <- vctrs::data_frame(x = c(1, 3, 4), y = I(list(0L, 100:101, -1L)))

  out <- rows_upsert(x, y, "x")
  expect_identical(out$y, I(list(0L, 3:4, 100:101, -1L)))
})

test_that("rows_upsert() casts keys to their common type for matching but retains `x` type", {
  x <- vctrs::data_frame(key = 1L, value = 2)
  y <- vctrs::data_frame(key = c(2, 1), value = c(1.5, 2.5))

  out <- rows_upsert(x, y, "key")
  expect_identical(out$key, c(1L, 2L))
  expect_identical(out$value, c(2.5, 1.5))

  y <- vctrs::data_frame(key = "x", value = 1.5)

  expect_snapshot({
    (expect_error(rows_upsert(x, y, "key")))
  })
})

test_that("rows_upsert() casts keys to the type of `x`", {
  x <- vctrs::data_frame(key = 1L, value = 2)
  y <- vctrs::data_frame(key = 1.5, value = 1.5)

  expect_snapshot({
    (expect_error(rows_upsert(x, y, "key")))
  })
})

test_that("rows_upsert() casts values to the type of `x`", {
  x <- vctrs::data_frame(key = 1, value = 2L)
  y <- vctrs::data_frame(key = 1, value = 1.5)

  expect_snapshot({
    (expect_error(rows_upsert(x, y, "key")))
  })
})

# ------------------------------------------------------------------------------

test_that("rows_delete() works", {
  data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)

  expect_identical(
    rows_delete(data, tibble(a = 2:3), by = "a"),
    data[1, ]
  )
})

test_that("rows_delete() ignores extra `y` columns, with a message", {
  x <- tibble(a = 1)
  y <- tibble(a = 1, b = 2)

  expect_snapshot({
    out <- rows_delete(x, y)
  })

  expect_identical(out, x[0,])

  expect_snapshot({
    out <- rows_delete(x, y, by = "a")
  })

  expect_identical(out, x[0,])
})

test_that("rows_delete() requires `y` keys to exist in `x` by default", {
  x <- tibble(a = 1, b = 2)
  y <- tibble(a = c(2, 1, 3), b = c(1, 1, 1))

  expect_snapshot((expect_error(rows_delete(x, y, "a"))))
})

test_that("rows_delete() allows `y` keys that don't exist in `x` to be ignored", {
  x <- tibble(a = 1, b = 2)
  y <- tibble(a = c(2, 1, 3))

  expect_identical(
    rows_delete(x, y, "a", unmatched = "ignore"),
    tibble(a = double(), b = double())
  )
})

test_that("rows_delete() allows `x` keys to be duplicated (#5553)", {
  x <- tibble(a = c(1, 2, 1, 3), b = c(NA, 3, 4, NA), c = letters[1:4])
  y <- tibble(a = c(1, 3))

  expect_identical(
    rows_delete(x, y, by = "a"),
    x[2,]
  )
})

test_that("rows_delete() allows `y` keys to be duplicated (#5553)", {
  x <- tibble(a = c(1, 2, 3), b = c(4, 5, 6))
  y <- tibble(a = c(1, 1))

  expect_identical(
    rows_delete(x, y, by = "a"),
    x[c(2, 3),]
  )
})

test_that("rows_delete() casts keys to their common type for matching but retains `x` type", {
  x <- vctrs::data_frame(key = c(1L, 2L), value = c("x", "y"))
  y <- vctrs::data_frame(key = 2)

  out <- rows_delete(x, y, "key")
  expect_identical(out$key, 1L)

  y <- vctrs::data_frame(key = "x", value = 1.5)

  expect_snapshot({
    (expect_error(rows_delete(x, y, "key")))
  })
})

# ------------------------------------------------------------------------------
# Common errors

test_that("rows_check_containment() checks that `y` columns are in `x`", {
  x <- tibble(a = 1)
  y <- tibble(a = 1, b = 2)

  expect_snapshot((expect_error(rows_check_containment(x, y))))
})

test_that("rows_check_by() checks that `y` has at least 1 column before using it (#6061)", {
  y <- tibble()

  expect_snapshot((expect_error(rows_check_by(by = NULL, y = y))))
})

test_that("rows_check_by() uses the first column from `y` by default, with a message", {
  y <- tibble(a = 1, b = 2)

  expect_snapshot(
    by <- rows_check_by(by = NULL, y = y)
  )

  expect_identical(by, "a")
})

test_that("rows_check_by() validates `by`", {
  y <- tibble(x = 1)

  expect_snapshot({
    (expect_error(rows_check_by(by = 1, y = y)))
    (expect_error(rows_check_by(by = character(), y = y)))
    (expect_error(rows_check_by(by = c(x = "y"), y = y)))
  })
})

test_that("rows_select_key() selects the key columns", {
  x <- tibble(x = 1, y = 2, z = 3)

  expect_identical(
    rows_select_key(x, c("z", "x"), "x"),
    x[c("z", "x")]
  )
})

test_that("rows_select_key() checks that all `by` columns are in `x`", {
  x <- tibble(x = 1)

  expect_snapshot({
    (expect_error(rows_select_key(x, "y", arg = "x")))
    (expect_error(rows_select_key(x, c("y", "x", "z"), arg = "y")))
  })
})

test_that("rows_select_key() optionally requires uniqueness", {
  x <- tibble(x = c(1, 1, 1), y = c(2, 3, 2), z = c(1, 2, 3))

  expect_identical(rows_select_key(x, "z", arg = "x", unique = TRUE), x["z"])

  expect_snapshot({
    (expect_error(rows_select_key(x, "x", arg = "x", unique = TRUE)))
    (expect_error(rows_select_key(x, c("x", "y"), arg = "y", unique = TRUE)))
  })
})
