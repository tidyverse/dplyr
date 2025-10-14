test_that("formula interface works as expected", {
  x <- c(1, 2, 0, NA, 0, NA, 5)
  y <- seq_along(x)
  z <- as.character(y)

  expect_identical(
    recode_values(x, 0 ~ "zero", NA ~ z, default = "default"),
    c("default", "default", "zero", "4", "zero", "6", "default")
  )
})

test_that("from/to vector interface works as expected", {
  x <- c("a", "b", "a", "c", NA, "d", NA, "e")

  # Lookup table
  # fmt: skip
  table <- tribble(
    ~from, ~to,
    "a", "A",
    "b", "B",
    "c", "C"
  )

  expect_identical(
    recode_values(x, from = table$from, to = table$to),
    c("A", "B", "A", "C", NA, NA, NA, NA)
  )
  expect_identical(
    recode_values(x, from = table$from, to = table$to, default = "0"),
    c("A", "B", "A", "C", "0", "0", "0", "0")
  )
  expect_identical(
    replace_values(x, from = table$from, to = table$to),
    c("A", "B", "A", "C", NA, "d", NA, "e")
  )
})

test_that("from/to list of vectors interface works as expected", {
  x <- c("a", "b", "a", "c")

  # Lookup table
  # fmt: skip
  table <- tribble(
    ~from, ~to,
    c("a", "b"), "AB",
    c("c"), "C"
  )
  # `from` is a list, `to` is not
  expect_identical(table$from, list(c("a", "b"), "c"))
  expect_identical(table$to, c("AB", "C"))
  expect_identical(
    recode_values(x, from = table$from, to = table$to),
    c("AB", "AB", "AB", "C")
  )
  expect_identical(
    replace_values(x, from = table$from, to = table$to),
    c("AB", "AB", "AB", "C")
  )

  # Lookup table
  # fmt: skip
  table <- tribble(
    ~from, ~to,
    "a", 1:4,
    "b", 5:8,
    "c", 9:12
  )
  # `to` is a list, `from` is not
  expect_identical(table$from, c("a", "b", "c"))
  expect_identical(table$to, list(1:4, 5:8, 9:12))
  expect_identical(
    recode_values(x, from = table$from, to = table$to),
    c(1L, 6L, 3L, 12L)
  )

  # Lookup table
  # fmt: skip
  table <- tribble(
    ~from, ~to,
    c("a", "b"), 1:4,
    c("c"), 5:8
  )
  # `from` is a list, `to` is a list
  expect_identical(table$from, list(c("a", "b"), "c"))
  expect_identical(table$to, list(1:4, 5:8))
  expect_identical(
    recode_values(x, from = table$from, to = table$to),
    c(1L, 2L, 3L, 8L)
  )
})

test_that("when `from` is a list, `to` must recycle to the same size as that list", {
  expect_identical(
    recode_values(1:2, from = list(1, 2:3), to = 0),
    c(0, 0)
  )
  expect_snapshot(error = TRUE, {
    recode_values(1, from = list(1, 2, 3), to = c(1, 2))
  })
})

test_that("`NA` is considered unmatched unless handled explicitly", {
  # Like `inner_join(unmatched = "error")`.

  # We think it would be exponentially more complex to try and add some kind of
  # additional `missing` argument that handles missing values separately from
  # `unmatched` values. It's kind of nice that you have to be explicit in your
  # lookup table about whether or not you are expecting a missing value when
  # you've opted into the strict world of `unmatched = "error"`.

  x <- c("a", "b", "a", NA, "c")

  # Lookup table
  # fmt: skip
  table <- tribble(
    ~from, ~to,
    "a", "A",
    "b", "B",
    "c", "C"
  )

  expect_snapshot(error = TRUE, {
    recode_values(x, from = table$from, to = table$to, unmatched = "error")
  })

  table <- add_row(table, from = NA, to = NA)

  expect_identical(
    recode_values(x, from = table$from, to = table$to, unmatched = "error"),
    c("A", "B", "A", NA, "C")
  )
})

test_that("`NA` is matched exactly", {
  # With logical `NA`
  x <- c(1, NA)

  expect_identical(recode_values(x, NA ~ 0), c(NA, 0))
  expect_identical(recode_values(x, from = NA, to = 0), c(NA, 0))

  expect_identical(replace_values(x, NA ~ 0), c(1, 0))
  expect_identical(replace_values(x, from = NA, to = 0), c(1, 0))

  # With typed `NA`
  expect_identical(recode_values(x, NA_real_ ~ 0), c(NA, 0))
  expect_identical(recode_values(x, from = NA_real_, to = 0), c(NA, 0))

  expect_identical(replace_values(x, NA_real_ ~ 0), c(1, 0))
  expect_identical(replace_values(x, from = NA_real_, to = 0), c(1, 0))

  # `NA_real_` vs `NaN`
  x <- c(1, NA, NaN)
  expect_identical(
    recode_values(x, from = c(NA, NaN), to = c(2, 3)),
    c(NA, 2, 3)
  )
  expect_identical(
    replace_values(x, from = c(NA, NaN), to = c(2, 3)),
    c(1, 2, 3)
  )
})

test_that("`x` must be a vector", {
  x <- lm(1 ~ 1)

  expect_snapshot(error = TRUE, {
    recode_values(x, 1 ~ 1)
  })
  expect_snapshot(error = TRUE, {
    replace_values(x, 1 ~ 1)
  })
})

test_that("respects `ptype`", {
  expect_identical(
    recode_values(1, from = 1, to = 0L, ptype = double()),
    0
  )
  expect_identical(
    recode_values(1, from = 2, to = 3L, default = 0L, ptype = double()),
    0
  )

  expect_snapshot(error = TRUE, {
    recode_values(1, 1 ~ 0L, ptype = character())
  })
  # Error index is right when `NULL` is involved
  expect_snapshot(error = TRUE, {
    recode_values(1, 1 ~ "x", NULL, 2 ~ 0L, ptype = character())
  })

  expect_snapshot(error = TRUE, {
    recode_values(1, from = 1, to = 0L, ptype = character())
  })
  expect_snapshot(error = TRUE, {
    recode_values(1, from = 1, to = "x", default = 0L, ptype = character())
  })
})

test_that("`replace_values()` is type stable on `x`", {
  # Common type would be double, but we use type of `x`
  expect_identical(
    replace_values(1:2, from = 1L, to = 0),
    c(0L, 2L)
  )

  x <- factor(c("a", "b"))

  # Common type would be character, but we use type of `x`
  expect_identical(
    replace_values(x, from = "a", to = "b"),
    factor(c("b", "b"), levels = c("a", "b"))
  )

  expect_snapshot(error = TRUE, {
    replace_values(x, "c" ~ "b")
  })
  expect_snapshot(error = TRUE, {
    replace_values(x, from = "c", to = "b")
  })

  expect_snapshot(error = TRUE, {
    replace_values(x, "a" ~ "c")
  })
  expect_snapshot(error = TRUE, {
    replace_values(x, from = "a", to = "c")
  })

  # Error index is right when `NULL` is involved
  expect_snapshot(error = TRUE, {
    replace_values(x, "a" ~ "b", NULL, "b" ~ "c")
  })
})

test_that("respects `default`", {
  expect_identical(
    recode_values(1:3, 2 ~ 0, default = 1),
    c(1, 0, 1)
  )
  expect_identical(
    recode_values(1:3, 2 ~ 0, default = 4:6),
    c(4, 0, 6)
  )
})

test_that("`default` is part of `ptype` determination", {
  # Common type of double
  expect_identical(
    recode_values(1, from = 1, to = 0L, default = 1),
    0
  )
  expect_snapshot(error = TRUE, {
    recode_values(1, from = 1, to = 0L, default = "x")
  })
})

test_that("`default` has its size checked", {
  expect_snapshot(error = TRUE, {
    recode_values(1:3, 1 ~ 0, default = 1:5)
  })
})

test_that("treats list `from` and `to` as lists of vectors", {
  # To align with what the `...` interface allows.
  # Use the vctrs interface if you want `from` and `to` lists treated as vectors.
  x <- 1:4
  a <- c(1L, 3L)
  b <- 4L

  expect_identical(
    recode_values(x, from = list(a, b), to = list(0L, 5L)),
    c(0L, NA, 0L, 5L)
  )
  expect_identical(
    replace_values(x, from = list(a, b), to = list(0L, 5L)),
    c(0L, 2L, 0L, 5L)
  )

  # Notice how `from` and `to` are "just as powerful" as the formula interface
  # because we treat lists this way. That's the invariant we are going for here.
  expect_identical(
    recode_values(x, from = list(a, b), to = list(0L, 5L)),
    recode_values(x, a ~ 0L, b ~ 5L)
  )
  expect_identical(
    replace_values(x, from = list(a, b), to = list(0L, 5L)),
    replace_values(x, a ~ 0L, b ~ 5L)
  )

  # To treat `from` and `to` lists as vectors, use vctrs
  x <- list(1, 2, 3:4, 5)
  from <- list(3:4, 2)
  to <- list(1L, 6:7)

  expect_snapshot(error = TRUE, {
    recode_values(x, from = from, to = to)
  })
  expect_identical(
    vec_recode_values(x, from = from, to = to),
    list(NULL, 6:7, 1L, NULL)
  )
})

test_that("`...` must be unnamed", {
  # Better than `case_when()`!
  expect_snapshot(error = TRUE, {
    recode_values(1, foo = 1 ~ 2)
  })
  expect_snapshot(error = TRUE, {
    replace_values(1, foo = 1 ~ 2)
  })
})

test_that("`...` must contain two sided formulas", {
  expect_snapshot(error = TRUE, {
    recode_values(1, 1 ~ 1, 2)
  })
  expect_snapshot(error = TRUE, {
    replace_values(1, 1 ~ 1, 2)
  })

  expect_snapshot(error = TRUE, {
    recode_values(1, 1 ~ 1, ~2)
  })
  expect_snapshot(error = TRUE, {
    replace_values(1, 1 ~ 1, ~2)
  })
})

test_that("throws correct errors based on all combinations of `...` and `from` and `to`", {
  # None of `...` and `from` or `to`
  expect_snapshot(error = TRUE, recode_values(1))
  # `replace_values()` is a no-op here like `replace_when()`, see other tests
  # expect_snapshot(error = TRUE, replace_values(1))

  # Both `...` and `from`
  expect_snapshot(error = TRUE, recode_values(1, 1 ~ 2, from = 1))
  expect_snapshot(error = TRUE, replace_values(1, 1 ~ 2, from = 1))

  # `from` but not `to`
  expect_snapshot(error = TRUE, recode_values(1, from = 1))
  expect_snapshot(error = TRUE, replace_values(1, from = 1))

  # `to` but not `from`
  expect_snapshot(error = TRUE, recode_values(1, to = 1))
  expect_snapshot(error = TRUE, replace_values(1, to = 1))
})

test_that("replace_values() is a no-op with no `...` or `from` and `to`", {
  # Like `replace_when()`
  expect_identical(replace_values(1), 1)
})

test_that("recode_values() takes names from inputs", {
  expect_identical(
    recode_values(
      c(a = 1, b = 2),
      c(c = 1) ~ c(d = 0),
      default = c(e = 3)
    ),
    c(d = 0, e = 3)
  )
  expect_identical(
    recode_values(
      c(a = 1, b = 2),
      from = c(c = 1),
      to = c(d = 0),
      default = c(e = 3)
    ),
    c(d = 0, e = 3)
  )
})

test_that("replace_values() takes names from `x`", {
  expect_identical(
    replace_values(
      c(a = 1, b = 2),
      c(c = 1) ~ c(d = 0)
    ),
    c(a = 0, b = 2)
  )
  expect_identical(
    replace_values(
      c(a = 1, b = 2),
      from = c(c = 1),
      to = c(d = 0)
    ),
    c(a = 0, b = 2)
  )
})
