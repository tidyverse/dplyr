test_that("empty slice returns input", {
  df <- tibble(x = 1:3)
  expect_equal(slice(df), df)
})

test_that("slice handles numeric input (#226)", {
  g <- mtcars %>% arrange(cyl) %>% group_by(cyl)
  res <- g %>% slice(1)
  expect_equal(nrow(res), 3)
  expect_equal(res, g %>% filter(row_number() == 1L))

  expect_equal(
    mtcars %>% slice(1),
    mtcars %>% filter(row_number() == 1L)
  )
})

test_that("slice silently ignores out of range values (#226)", {
  expect_equal(slice(mtcars, c(2, 100)), slice(mtcars, 2))

  g <- group_by(mtcars, cyl)
  expect_equal(slice(g, c(2, 100)), slice(g, 2))
})

test_that("slice works with negative indices", {
  res <- slice(mtcars, -(1:2))
  exp <- tail(mtcars, -2)
  expect_equal(res, exp, ignore_attr = TRUE)
})

test_that("slice works with grouped data", {
  g <- mtcars %>% arrange(cyl) %>% group_by(cyl)

  res <- slice(g, 1:2)
  exp <- filter(g, row_number() < 3)
  expect_equal(res, exp)

  res <- slice(g, -(1:2))
  exp <- filter(g, row_number() >= 3)
  expect_equal(res, exp)

  g <- group_by(data.frame(x = c(1, 1, 2, 2, 2)), x)
  expect_equal(group_keys(slice(g, 3, .preserve = TRUE))$x, c(1, 2))
  expect_equal(group_keys(slice(g, 3, .preserve = FALSE))$x, 2)
})

test_that("slice gives correct rows (#649)", {
  a <- tibble(value = paste0("row", 1:10))
  expect_equal(slice(a, 1:3)$value, paste0("row", 1:3))
  expect_equal(slice(a, c(4, 6, 9))$value, paste0("row", c(4, 6, 9)))

  a <- tibble(
    value = paste0("row", 1:10),
    group = rep(1:2, each = 5)
  ) %>%
    group_by(group)

  expect_equal(slice(a, 1:3)$value, paste0("row", c(1:3, 6:8)))
  expect_equal(slice(a, c(2, 4))$value, paste0("row", c(2, 4, 7, 9)))
})

test_that("slice handles NA (#1235)", {
  df <- tibble(x = 1:3)
  expect_equal(nrow(slice(df, NA_integer_)), 0L)
  expect_equal(nrow(slice(df, c(1L, NA_integer_))), 1L)
  expect_equal(nrow(slice(df, c(-1L, NA_integer_))), 2L)

  df <- tibble(x = 1:4, g = rep(1:2, 2)) %>% group_by(g)
  expect_equal(nrow(slice(df, c(1, NA))), 2)
  expect_equal(nrow(slice(df, c(-1, NA))), 2)
})

test_that("slice handles logical NA (#3970)", {
  df <- tibble(x = 1:3)
  expect_equal(nrow(slice(df, NA)), 0L)
})

test_that("slice handles empty data frames (#1219)", {
  df <- data.frame(x = numeric())
  res <- df %>% slice(1:3)
  expect_equal(nrow(res), 0L)
  expect_equal(names(res), "x")
})

test_that("slice works fine if n > nrow(df) (#1269)", {
  by_slice <- mtcars %>% arrange(cyl) %>%  group_by(cyl)
  slice_res <- by_slice  %>% slice(8)
  filter_res <- by_slice %>% group_by(cyl) %>% filter(row_number() == 8)
  expect_equal(slice_res, filter_res)
})

test_that("slice strips grouped indices (#1405)", {
  res <- mtcars %>% group_by(cyl) %>% slice(1) %>% mutate(mpgplus = mpg + 1)
  expect_equal(nrow(res), 3L)
  expect_equal(group_rows(res), list_of(1L, 2L, 3L))
})

test_that("slice works with zero-column data frames (#2490)", {
  expect_equal(
    tibble(a = 1:3) %>% select(-a) %>% slice(1) %>% nrow(),
    1L
  )
})

test_that("slice correctly computes positive indices from negative indices (#3073)", {
  x <- tibble(y = 1:10)
  expect_identical(slice(x, -10:-30), tibble(y = 1:9))
})

test_that("slice handles raw matrices", {
  df <- tibble(a = 1:4, b = matrix(as.raw(1:8), ncol = 2))
  expect_identical(
    slice(df, 1:2)$b,
    matrix(as.raw(c(1, 2, 5, 6)), ncol = 2)
  )
})

test_that("slice on ungrouped data.frame (not tibble) does not enforce tibble", {
  expect_equal(class(slice(mtcars, 2)), "data.frame")
  expect_equal(class(slice(mtcars, -2)), "data.frame")
  expect_equal(class(slice(mtcars, NA)), "data.frame")
})

test_that("slice skips 0 (#3313)", {
  d <- tibble(x = 1:5, y = LETTERS[1:5], g = 1)
  expect_identical(slice(d, 0), slice(d, integer(0)))
  expect_identical(slice(d, c(0, 1)), slice(d, 1))
  expect_identical(slice(d, c(0, 1, 2)), slice(d, c(1, 2)))

  expect_identical(slice(d, c(-1, 0)), slice(d, -1))
  expect_identical(slice(d, c(0, -1)), slice(d, -1))

  d <- group_by(d, g)
  expect_identical(slice(d, 0), slice(d, integer(0)))
  expect_identical(slice(d, c(0, 1)), slice(d, 1))
  expect_identical(slice(d, c(0, 1, 2)), slice(d, c(1, 2)))

  expect_identical(slice(d, c(-1, 0)), slice(d, -1))
  expect_identical(slice(d, c(0, -1)), slice(d, -1))
})

test_that("slice accepts ... (#3804)", {
  expect_equal(slice(mtcars, 1, 2), slice(mtcars, 1:2))
  expect_equal(slice(mtcars, 1, n()), slice(mtcars, c(1, nrow(mtcars))))

  g <- mtcars %>% group_by(cyl)
  expect_equal(slice(g, 1, n()), slice(g, c(1, n())))
})

test_that("slice does not evaluate the expression in empty groups (#1438)", {
  res <- mtcars %>%
    group_by(cyl) %>%
    filter(cyl==6) %>%
    slice(1:2)
  expect_equal(nrow(res), 2L)

  expect_error(
    res <- mtcars %>% group_by(cyl) %>% filter(cyl==6) %>% sample_n(size=3),
    NA
  )
  expect_equal(nrow(res), 3L)
})

test_that("slice() handles matrix and data frame columns (#3630)", {
  df <- tibble(
    x = 1:2,
    y = matrix(1:4, ncol = 2),
    z = data.frame(A = 1:2, B = 3:4)
  )
  expect_equal(slice(df, 1), df[1, ])
  expect_equal(slice(df, 1), df[1, ])
  expect_equal(slice(df, 1), df[1, ])

  gdf <- group_by(df, x)
  expect_equal(slice(gdf, 1), gdf)
  expect_equal(slice(gdf, 1), gdf)
  expect_equal(slice(gdf, 1), gdf)

  gdf <- group_by(df, y)
  expect_equal(slice(gdf, 1), gdf)
  expect_equal(slice(gdf, 1), gdf)
  expect_equal(slice(gdf, 1), gdf)

  gdf <- group_by(df, z)
  expect_equal(slice(gdf, 1), gdf)
  expect_equal(slice(gdf, 1), gdf)
  expect_equal(slice(gdf, 1), gdf)
})

# Slice variants ----------------------------------------------------------

test_that("slice_sample() handles n= and prop=", {
  df <- data.frame(a = 1)

  expect_equal(
    df %>% slice_sample(n = 4, replace = TRUE),
    df %>% slice(rep(1, 4))
  )

  expect_equal(
    df %>% slice_sample(prop = 4, replace = TRUE),
    df %>% slice(rep(1, 4))
  )

  expect_snapshot({
    (expect_error(
      df %>% slice_sample(n = -1)
    ))
    (expect_error(
      df %>% slice_sample(prop = -1)
    ))

    (expect_error(
      df %>% slice_sample(n = 4, replace = FALSE)
    ))

    (expect_error(
      df %>% slice_sample(prop = 4, replace = FALSE)
    ))
  })
})

test_that("functions silently truncate results", {
  df <- data.frame(x = 1:5)

  expect_equal(df %>% slice_head(n = 6) %>% nrow(), 5)
  expect_equal(df %>% slice_tail(n = 6) %>% nrow(), 5)
  expect_equal(df %>% slice_min(x, n = 6) %>% nrow(), 5)
  expect_equal(df %>% slice_max(x, n = 6) %>% nrow(), 5)
  expect_equal(df %>% slice_head(n = -6) %>% nrow(), 0)
  expect_equal(df %>% slice_tail(n = -6) %>% nrow(), 0)
  expect_equal(df %>% slice_min(x, n = -6) %>% nrow(), 0)
  expect_equal(df %>% slice_max(x, n = -6) %>% nrow(), 0)
})

test_that("proportion computed correctly", {
  df <- data.frame(x = 1:10)

  expect_equal(df %>% slice_head(prop = 0.11) %>% nrow(), 1)
  expect_equal(df %>% slice_tail(prop = 0.11) %>% nrow(), 1)
  expect_equal(df %>% slice_sample(prop = 0.11) %>% nrow(), 1)
  expect_equal(df %>% slice_min(x, prop = 0.11) %>% nrow(), 1)
  expect_equal(df %>% slice_max(x, prop = 0.11) %>% nrow(), 1)
  expect_equal(df %>% slice_min(x, prop = 0.11, with_ties = FALSE) %>% nrow(), 1)
  expect_equal(df %>% slice_max(x, prop = 0.11, with_ties = FALSE) %>% nrow(), 1)
})

test_that("min and max return ties by default", {
  df <- data.frame(x = c(1, 1, 1, 2, 2))
  expect_equal(df %>% slice_min(x) %>% nrow(), 3)
  expect_equal(df %>% slice_max(x) %>% nrow(), 2)

  expect_equal(df %>% slice_min(x, with_ties = FALSE) %>% nrow(), 1)
  expect_equal(df %>% slice_max(x, with_ties = FALSE) %>% nrow(), 1)
})

test_that("min and max reorder results", {
  df <- data.frame(id = 1:4, x = c(2, 3, 1, 2))

  expect_equal(df %>% slice_min(x, n = 2) %>% pull(id), c(3, 1, 4))
  expect_equal(df %>% slice_min(x, n = 2, with_ties = FALSE) %>% pull(id), c(3, 1))
  expect_equal(df %>% slice_max(x, n = 2) %>% pull(id), c(2, 1, 4))
  expect_equal(df %>% slice_max(x, n = 2, with_ties = FALSE) %>% pull(id), c(2, 1))
})

test_that("min and max ignore NA's (#4826)", {
  df <- data.frame(id = 1:4, x = c(2, NA, 1, 2), y = c(NA, NA, NA, NA))

  expect_equal(df %>% slice_min(x, n = 2) %>% pull(id), c(3, 1, 4))
  expect_equal(df %>% slice_min(y, n = 2) %>% nrow(), 0)
  expect_equal(df %>% slice_max(x, n = 2) %>% pull(id), c(1, 4))
  expect_equal(df %>% slice_max(y, n = 2) %>% nrow(), 0)
})

test_that("arguments to sample are passed along", {
  df <- data.frame(x = 1:100, wt = c(1, rep(0, 99)))

  expect_equal(df %>% slice_sample(n = 1, weight_by = wt) %>% pull(x), 1)
  expect_equal(df %>% slice_sample(n = 2, weight_by = wt, replace = TRUE) %>% pull(x), c(1, 1))
})

test_that("slice() handles matrices", {
  df <- data.frame(x = 1)
  expect_identical(
    slice(df, 1),
    slice(df, matrix(1))
  )
})

test_that("slice() gives meaningfull errors", {
  df <- data.frame(x = 1:2)
  gdf <- group_by(df, x)

  expect_snapshot({
    (expect_error(
      slice(df, matrix(c(1, 2), ncol = 2))
    ))
    (expect_error(
      slice(gdf, matrix(c(1, 2), ncol = 2))
    ))

    (expect_error(
      slice(df, "a")
    ))
    (expect_error(
      slice(gdf, "a")
    ))

    (expect_error(
      slice(df, c(1, -1))
    ))
    (expect_error(
      slice(gdf, c(1, -1))
    ))
  })

})

test_that("slice_*() checks that `n=` is explicitly named", {
  df <- data.frame(x = 1:10)
  expect_snapshot({
    (expect_error(
      slice_head(df, 5)
    ))
    (expect_error(
      slice_tail(df, 5)
    ))
    (expect_error(
      slice_min(df, x, 5)
    ))
    (expect_error(
      slice_max(df, x, 5)
    ))
    (expect_error(
      slice_sample(df, 5)
    ))
  })
})

test_that("slice_*() not confusing `n` (#6089)", {
  df <- data.frame(x = 1:10, n = 10:1, g = rep(1:2, each = 5))
  expect_error(slice_max(df, order_by = n), NA)
  expect_error(slice_min(df, order_by = n), NA)
  expect_error(slice_sample(df, weight_by = n, n = 1L), NA)

  df <- group_by(df, g)
  expect_error(slice_max(df, order_by = n), NA)
  expect_error(slice_min(df, order_by = n), NA)
  expect_error(slice_sample(df, weight_by = n, n = 1L), NA)
})

test_that("slice_*() checks that for empty `...", {
  df <- data.frame(x = 1:10)
  expect_snapshot({
    (expect_error(
      slice_head(df, 5, 2)
    ))
    (expect_error(
      slice_tail(df, 5, 2)
    ))
    (expect_error(
      slice_min(df, x, 5, 2)
    ))
    (expect_error(
      slice_max(df, x, 5, 2)
    ))
    (expect_error(
      slice_sample(df, 5, 2)
    ))
  })

  expect_snapshot({
    (expect_error(
      slice_head(df, n = 5, 2)
    ))
    (expect_error(
      slice_tail(df, n = 5, 2)
    ))
    (expect_error(
      slice_min(df, x, n = 5, 2)
    ))
    (expect_error(
      slice_max(df, x, n = 5, 2)
    ))
    (expect_error(
      slice_sample(df, n = 5, 2)
    ))
  })

  expect_snapshot({
    (expect_error(
      slice_head(df, prop = .5, 2)
    ))
    (expect_error(
      slice_tail(df, prop = .5, 2)
    ))
    (expect_error(
      slice_min(df, x, prop = .5, 2)
    ))
    (expect_error(
      slice_max(df, x, prop = .5, 2)
    ))
    (expect_error(
      slice_sample(df, prop = .5, 2)
    ))
  })
})


test_that("slice_*() checks for constant n= and prop=", {
  df <- data.frame(x = 1:10)

  expect_snapshot({
    (expect_error(
      slice_head(df, n = n())
    ))
    (expect_error(
      slice_head(df, prop = n())
    ))

    (expect_error(
      slice_tail(df, n = n())
    ))
    (expect_error(
      slice_tail(df, prop = n())
    ))

    (expect_error(
      slice_min(df, x, n = n())
    ))
    (expect_error(
      slice_min(df, x, prop = n())
    ))

    (expect_error(
      slice_max(df, x, n = n())
    ))
    (expect_error(
      slice_max(df, x, prop = n())
    ))

    (expect_error(
      slice_sample(df, n = n())
    ))
    (expect_error(
      slice_sample(df, prop = n())
    ))
  })

})

test_that("slice_min/max() check size of `order_by=` (#5922)", {
  expect_snapshot({
    (expect_error(
      slice_min(data.frame(x = 1:10), 1:6)
    ))
    (expect_error(
      slice_max(data.frame(x = 1:10), 1:6)
    ))
  })
})

test_that("slice_sample() check size of `weight_by=` (#5922)", {
  expect_snapshot({
    (expect_error(
      slice_sample(data.frame(x = 1:10), n = 2, weight_by = 1:6)
    ))
  })
})

test_that("slice_sample() does not error on zero rows (#5729)", {
  df <- tibble(dummy = character(), weight = numeric(0))
  res <- expect_error(slice_sample(df, prop=0.5, weight_by = weight), NA)
  expect_equal(nrow(res), 0L)
})

test_that("slice_head/slice_tail correctly slice ungrouped df when n < 0", {
  df <- data.frame(x = 1:10)

  expect_equal(
    slice_head(df, n = -2),
    slice_head(df, n = nrow(df) - 2)
  )
  expect_equal(
    slice_tail(df, n = -2),
    slice_tail(df, n = nrow(df) - 2)
  )
})

test_that("slice_head,tail() handle n,prop = Inf", {
  df <- data.frame(x = 1)
  expect_identical(slice_head(df, n = Inf), df)
  expect_identical(slice_tail(df, n = Inf), df)
  expect_identical(slice_head(df, prop = Inf), df)
  expect_identical(slice_tail(df, prop = Inf), df)

  expect_identical(slice_head(df, n = -Inf), data.frame(x = numeric()))
  expect_identical(slice_tail(df, n = -Inf), data.frame(x = numeric()))
  expect_identical(slice_head(df, prop = -Inf), data.frame(x = numeric()))
  expect_identical(slice_tail(df, prop = -Inf), data.frame(x = numeric()))
})

test_that("slice_head/slice_tail correctly slice grouped df when n < 0", {
  df <- data.frame(x = 1:10, g = c(rep(1, 8), rep(2, 2))) %>% group_by(g)

  expect_equal(
    slice_head(df, n = -3),
    slice(df, rlang::seq2(1L, n() - 3))
  )
  expect_equal(
    n_groups(slice_head(df, n = -3)),
    1L
  )
  expect_equal(
    slice_tail(df, n = -3),
    slice(df, rlang::seq2(3 + 1, n()))
  )
  expect_equal(
    n_groups(slice_tail(df, n = -3)),
    1L
  )

})

test_that("Non-integer number of rows computed correctly", {
  expect_equal(get_slice_size(n = 1.6)(10), 1)
  expect_equal(get_slice_size(prop = 0.16)(10), 1)
  expect_equal(get_slice_size(n = -1.6)(10), 9)
  expect_equal(get_slice_size(prop = -0.16)(10), 9)
})

test_that("slice_helpers do call slice() and benefit from dispatch (#6084)", {
  local_methods(
    slice.noisy = function(.data, ..., .preserve = FALSE) {
      warning("noisy")
      NextMethod()
    }
  )

  noisy <- function(x) {
    class(x) <- c("noisy", class(x))
    x
  }

  df <- tibble(x = 1:10, g = rep(1:2, each = 5)) %>% group_by(g)

  expect_warning(slice(noisy(df), 1:2), "noisy")
  expect_warning(slice_sample(noisy(df), n = 2), "noisy")
  expect_warning(slice_head(noisy(df), n = 2), "noisy")
  expect_warning(slice_tail(noisy(df), n = 2), "noisy")
  expect_warning(slice_min(noisy(df), x, n = 2), "noisy")
  expect_warning(slice_max(noisy(df), x, n = 2), "noisy")
  expect_warning(sample_n(noisy(df), 2), "noisy")
  expect_warning(sample_frac(noisy(df), .5), "noisy")
})

# Errors ------------------------------------------------------------------

test_that("rename errors with invalid grouped data frame (#640)", {
  expect_snapshot({
    df <- tibble(x = 1:3)

    # User errors are labelled
    (expect_error(slice(mtcars, 1, 1 + "")))
    (expect_error(group_by(mtcars, cyl) %>% slice(1, 1 + "")))

    # Incompatible type
    (expect_error(slice(df, TRUE)))
    (expect_error(slice(df, FALSE)))
    (expect_error(slice(mtcars, 1, 1, "")))
    (expect_error(group_by(mtcars, cyl) %>% slice(1, 1, "")))

    # Mix of positive and negative integers
    (expect_error(mtcars %>% slice(c(-1, 2))))
    (expect_error(mtcars %>% slice(c(2:3, -1))))

    # n and prop are carefully validated
    (expect_error(slice_head(data.frame(), n = 1, prop = 1)))
    (expect_error(slice_tail(data.frame(), n = "a")))
    (expect_error(slide_head(data.frame(), prop = "a")))
    (expect_error(slice_head(data.frame(), n = n())))
    (expect_error(slice_head(data.frame(), prop = n())))
    (expect_error(slice_head(data.frame(), n = NA)))
    (expect_error(slice_head(data.frame(), prop = NA)))
  })
})
