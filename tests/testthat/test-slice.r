test_that("empty slice returns input", {
  df <- tibble(x = 1:3)
  expect_equal(slice(df), df)
})

test_that("slicing data.frame yields data.frame", {
  df <- data.frame(x = 1:3)
  expect_equal(slice(df, 1), data.frame(x = 1L))
})

test_that("slice keeps positive indices, ignoring out of range (#226)", {
  gf <- group_by(tibble(g = c(1, 2, 2, 3, 3, 3), id = 1:6), g)

  out <- slice(gf, 1)
  expect_equal(out$id, c(1, 2, 4))

  out <- slice(gf, 2)
  expect_equal(out$id, c(3, 5))
})

test_that("slice drops negative indices, ignoring out of range (#3073)", {
  gf <- group_by(tibble(g = c(1, 2, 2, 3, 3, 3), id = 1:6), g)

  out <- slice(gf, -1)
  expect_equal(out$id, c(3, 5, 6))

  out <- slice(gf, -(1:2))
  expect_equal(out$id, 6)
})

test_that("slice errors if positive and negative indices mixed", {
  expect_snapshot(error = TRUE, {
    slice(tibble(), 1, -1)
  })
})

test_that("slice ignores 0 and NA (#3313, #1235)", {
  gf <- group_by(tibble(g = c(1, 2, 2, 3, 3, 3), id = 1:6), g)

  out <- slice(gf, 0)
  expect_equal(out$id, integer())
  out <- slice(gf, 0, 1)
  expect_equal(out$id, c(1, 2, 4))

  out <- slice(gf, NA)
  expect_equal(out$id, integer())
  out <- slice(gf, NA, -1)
  expect_equal(out$id, c(3, 5, 6))
})

test_that("can slice with one-column matrix", {
  gf <- group_by(tibble(g = c(1, 2, 2, 3, 3, 3), id = 1:6), g)

  out <- slice(gf, matrix(1))
  expect_equal(out$id, c(1, 2, 4))
})

test_that("slice errors if index is not numeric", {
  expect_snapshot(error = TRUE, {
    slice(tibble(), "a")
  })
})

test_that("slice preserves groups iff requested", {
  gf <- group_by(tibble(g = c(1, 2, 2, 3, 3, 3), id = 1:6), g)

  out <- slice(gf, 2, 3)
  expect_equal(group_keys(out), tibble(g = c(2, 3)))
  expect_equal(group_rows(out), list_of(1, c(2, 3)))

  out <- slice(gf, 2, 3, .preserve = TRUE)
  expect_equal(group_keys(out), tibble(g = c(1, 2, 3)))
  expect_equal(group_rows(out), list_of(integer(), 1, c(2, 3)))
})

test_that("slice handles zero-row and zero-column inputs (#1219, #2490)", {
  df <- tibble(x = numeric())
  expect_equal(slice(df, 1), df)

  df <- tibble(.rows = 10)
  expect_equal(slice(df, 1), tibble(.rows = 1))
})

test_that("user errors are correctly labelled", {
  df <- tibble(x = 1:3)
  expect_snapshot(error = TRUE, {
    slice(df, 1 + "")
    slice(group_by(df, x), 1 + "")
  })
})

# Slice variants ----------------------------------------------------------

test_that("slice_helpers() call get_slice_size()", {
  df <- tibble(x = 1)

  expect_snapshot(error = TRUE, {
    slice_head(df, n = "a")
    slice_tail(df, n = "a")
    slice_min(df, x, n = "a")
    slice_max(df, x, n = "a")
    slice_sample(df, n= "a")
  })
})

test_that("get_slice_size() validates its inputs", {
  expect_snapshot(error = TRUE, {
    get_slice_size(n = 1, prop = 1)
    get_slice_size(n = foo)
    get_slice_size(prop = foo)
    get_slice_size(n = "a")
    get_slice_size(prop = "a")
  })
})

test_that("get_slice_size() converts proportions to numbers", {
  expect_equal(get_slice_size(prop = 0.5)(10), 5)
  expect_equal(get_slice_size(prop = -0.1)(10), 9)
  expect_equal(get_slice_size(prop = 1.1)(10), 11)
})

test_that("get_slice_size() converts negative to positive", {
  expect_equal(get_slice_size(n = -1)(10), 9)
  expect_equal(get_slice_size(prop = -0.5)(10), 5)
})

test_that("get_slice_size() rounds non-integers", {
  expect_equal(get_slice_size(n = 1.6)(10), 1)
  expect_equal(get_slice_size(prop = 0.16)(10), 1)
  expect_equal(get_slice_size(n = -1.6)(10), 9)
  expect_equal(get_slice_size(prop = -0.16)(10), 9)
})

test_that("functions silently truncate results", {
  # only test positive n because get_slice_size() converts all others

  df <- tibble(x = 1:5)
  expect_equal(nrow(slice_head(df, n = 6)), 5)
  expect_equal(nrow(slice_tail(df, n = 6)), 5)
  expect_equal(nrow(slice_min(df, x, n = 6)), 5)
  expect_equal(nrow(slice_max(df, x, n = 6)), 5)
  expect_equal(nrow(slice_sample(df, n = 6)), 5)
})

test_that("slice helpers with n = 0 return no rows", {
  df <- tibble(x = 1:5)
  expect_equal(nrow(slice_head(df, n = 0)), 0)
  expect_equal(nrow(slice_tail(df, n = 0)), 0)
  expect_equal(nrow(slice_min(df, x, n = 0)), 0)
  expect_equal(nrow(slice_max(df, x, n = 0)), 0)
  expect_equal(nrow(slice_sample(df, n = 0)), 0)
})

test_that("slice_*() doesn't look for `n` in data (#6089)", {
  df <- data.frame(x = 1:10, n = 10:1, g = rep(1:2, each = 5))
  expect_error(slice_max(df, order_by = n), NA)
  expect_error(slice_min(df, order_by = n), NA)
  expect_error(slice_sample(df, weight_by = n, n = 1L), NA)

  df <- group_by(df, g)
  expect_error(slice_max(df, order_by = n), NA)
  expect_error(slice_min(df, order_by = n), NA)
  expect_error(slice_sample(df, weight_by = n, n = 1L), NA)
})

test_that("slice_*() checks that `n=` is explicitly named and ... is empty", {
  # i.e. that every function calls check_slice_dots()

  df <- data.frame(x = 1:10)
  expect_snapshot(error = TRUE, {
    slice_head(df, 5)
    slice_tail(df, 5)
    slice_min(df, x, 5)
    slice_max(df, x, 5)
    slice_sample(df, 5)
  })

  expect_snapshot(error = TRUE, {
    slice_head(df, 5, 2)
    slice_tail(df, 5, 2)
    slice_min(df, x, 5, 2)
    slice_max(df, x, 5, 2)
    slice_sample(df, 5, 2)
  })
})

test_that("slice_helpers do call slice() and benefit from dispatch (#6084)", {
  local_methods(
    slice.noisy = function(.data, ..., .preserve = FALSE) {
      warning("noisy")
      NextMethod()
    }
  )

  nf <- tibble(x = 1:10, g = rep(1:2, each = 5)) %>% group_by(g)
  class(nf) <- c("noisy", class(nf))

  expect_warning(slice(nf, 1:2), "noisy")
  expect_warning(slice_sample(nf, n = 2), "noisy")
  expect_warning(slice_head(nf, n = 2), "noisy")
  expect_warning(slice_tail(nf, n = 2), "noisy")
  expect_warning(slice_min(nf, x, n = 2), "noisy")
  expect_warning(slice_max(nf, x, n = 2), "noisy")
  expect_warning(sample_n(nf, 2), "noisy")
  expect_warning(sample_frac(nf, .5), "noisy")
})

# slice_min/slice_max -----------------------------------------------------

test_that("min and max return ties by default", {
  df <- tibble(id = 1:5, x = c(1, 1, 1, 2, 2))
  expect_equal(slice_min(df, x)$id, c(1, 2, 3))
  expect_equal(slice_max(df, x)$id, c(4, 5))

  expect_equal(slice_min(df, x, with_ties = FALSE)$id, 1)
  expect_equal(slice_max(df, x, with_ties = FALSE)$id, 4)
})

test_that("min and max reorder results", {
  df <- data.frame(id = 1:4, x = c(2, 3, 1, 2))

  expect_equal(slice_min(df, x, n = 2)$id, c(3, 1, 4))
  expect_equal(slice_max(df, x, n = 2)$id, c(2, 1, 4))

  expect_equal(slice_min(df, x, n = 2, with_ties = FALSE)$id, c(3, 1))
  expect_equal(slice_max(df, x, n = 2, with_ties = FALSE)$id, c(2, 1))
})

test_that("min and max include NAs when appropriate", {
  df <- tibble(id = 1:3, x = c(1, NA, NA))
  expect_equal(slice_min(df, x, n = 1)$id, 1)
  expect_equal(slice_max(df, x, n = 1)$id, 1)

  expect_equal(slice_min(df, x, n = 2)$id, c(1, 2, 3))
  expect_equal(slice_min(df, x, n = 2, with_ties = FALSE)$id, c(1, 2))

  df <- tibble(id = 1:4, x = NA)
  expect_equal(slice_min(df, x, n = 2, na_rm = TRUE)$id, integer())
  expect_equal(slice_max(df, x, n = 2, na_rm = TRUE)$id, integer())
})

test_that("min and max ignore NA's when requested (#4826)", {
  df <- tibble(id = 1:4, x = c(2, NA, 1, 2))
  expect_equal(slice_min(df, x, n = 2, na_rm = TRUE)$id, c(3, 1, 4))
  expect_equal(slice_max(df, x, n = 2, na_rm = TRUE)$id, c(1, 4))

  # Check with list to confirm use full vctrs support
  df <- tibble(id = 1:4, x = list(NULL, 1, NULL, NULL))
  expect_equal(slice_min(df, x, n = 2, na_rm = TRUE)$id, 2)
  expect_equal(slice_max(df, x, n = 2, na_rm = TRUE)$id, 2)

  # Drop when any element is missing
  df <- tibble(id = 1:3, a = c(1, 2, NA), b = c(2, NA, NA))
  expect_equal(slice_min(df, tibble(a, b), n = 3, na_rm = TRUE)$id, 1)
  expect_equal(slice_max(df, tibble(a, b), n = 3, na_rm = TRUE)$id, 1)
})

test_that("slice_min/max() can order by multiple variables (#6176)", {
  df <- tibble(id = 1:4, x = 1, y = c(1, 4, 2, 3))
  expect_equal(slice_min(df, tibble(x, y), n = 1)$id, 1)
  expect_equal(slice_max(df, tibble(x, y), n = 1)$id, 2)
})

test_that("slice_min/max() check size of `order_by=` (#5922)", {
  expect_snapshot(error = TRUE, {
    slice_min(data.frame(x = 1:10), 1:6)
    slice_max(data.frame(x = 1:10), 1:6)
  })
})

test_that("slice_min/max() validate simple arguments", {
  expect_snapshot(error = TRUE, {
    slice_min(data.frame(x = 1:10))
    slice_max(data.frame(x = 1:10))

    slice_min(data.frame(x = 1:10), x, with_ties = 1)
    slice_max(data.frame(x = 1:10), x, with_ties = 1)

    slice_min(data.frame(x = 1:10), x, na_rm = 1)
    slice_max(data.frame(x = 1:10), x, na_rm = 1)
  })
})

# slice_sample ------------------------------------------------------------

test_that("slice_sample() respects weight_by and replaces", {
  df <- tibble(x = 1:100, wt = c(1, rep(0, 99)))

  out <- slice_sample(df, n = 1, weight_by = wt)
  expect_equal(out$x, 1)

  out <- slice_sample(df, n = 2, weight_by = wt, replace = TRUE)
  expect_equal(out$x, c(1, 1))
})

test_that("slice_sample() checks size of `weight_by=` (#5922)", {
  df <- tibble(x = 1:10)
  expect_snapshot(slice_sample(df, n = 2, weight_by = 1:6), error = TRUE)
})

test_that("slice_sample() works with zero-row data frame (#5729)", {
  df <- tibble(x = character(), w = numeric())
  out <- slice_sample(df, prop = 0.5, weight_by = w)
  expect_equal(out, df)
})

test_that("`slice_sample()` validates `replace`", {
  df <- tibble()
  expect_snapshot(error = TRUE, {
    slice_sample(df, replace = 1)
    slice_sample(df, replace = NA)
  })
})

test_that("slice_sample() handles n= and prop=", {
  gf <- group_by(tibble(a = 1, b = 1), a)
  expect_equal(slice_sample(gf, n = 3, replace = TRUE), gf[c(1, 1, 1), ])
  expect_equal(slice_sample(gf, prop = 3, replace = TRUE), gf[c(1, 1, 1), ])

  # Unlike other helpers, can't supply negative values
  expect_snapshot(error = TRUE, {
    slice_sample(gf, n = -1)
    slice_sample(gf, prop = -1)
  })
})

# slice_head/slice_tail ---------------------------------------------------

test_that("slice_head/slice_tail keep positive values", {
  gf <- group_by(tibble(g = c(1, 2, 2, 3, 3, 3), id = 1:6), g)

  expect_equal(slice_head(gf, n = 1)$id, c(1, 2, 4))
  expect_equal(slice_head(gf, n = 2)$id, c(1, 2, 3, 4, 5))

  expect_equal(slice_tail(gf, n = 1)$id, c(1, 3, 6))
  expect_equal(slice_tail(gf, n = 2)$id, c(1, 2, 3, 5, 6))
})

test_that("slice_head/slice_tail drop from opposite end when n/prop negative", {
  gf <- group_by(tibble(g = c(1, 2, 2, 3, 3, 3), id = 1:6), g)

  expect_equal(slice_head(gf, n = -1)$id, c(2, 4, 5))
  expect_equal(slice_head(gf, n = -2)$id, 4)

  expect_equal(slice_tail(gf, n = -1)$id, c(3, 5, 6))
  expect_equal(slice_tail(gf, n = -2)$id, 6)
})

test_that("slice_head/slice_tail handle infinite n/prop", {
  df <- tibble(x = 1)
  expect_identical(slice_head(df, n = Inf), df)
  expect_identical(slice_tail(df, n = Inf), df)
  expect_identical(slice_head(df, n = -Inf), df[0, ])
  expect_identical(slice_tail(df, n = -Inf), df[0, ])

  expect_identical(slice_head(df, prop = Inf), df)
  expect_identical(slice_tail(df, prop = Inf), df)
  expect_identical(slice_head(df, prop = -Inf), df[0, ])
  expect_identical(slice_tail(df, prop = -Inf), df[0, ])
})
