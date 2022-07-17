test_that("works with data frames", {
  conditions <- list(
    c(FALSE, TRUE, FALSE, FALSE),
    c(TRUE, TRUE, FALSE, FALSE),
    c(FALSE, TRUE, FALSE, TRUE)
  )
  values <- list(
    vctrs::data_frame(x = 1, y = 2),
    vctrs::data_frame(x = 3, y = 4),
    vctrs::data_frame(x = 3:6, y = 4:7)
  )

  out <- vec_case_when(conditions, values)

  expect_identical(
    out,
    vctrs::data_frame(
      x = c(3, 1, NA, 6),
      y = c(4, 2, NA, 7)
    )
  )
})

test_that("first `TRUE` case wins", {
  conditions <- list(
    c(TRUE, FALSE),
    c(TRUE, TRUE),
    c(TRUE, TRUE)
  )
  values <- list(
    1,
    2,
    3
  )

  expect_identical(
    vec_case_when(conditions, values),
    c(1, 2)
  )
})

test_that("can replace missing values by catching with `is.na()`", {
  x <- c(1:3, NA)

  conditions <- list(
    x <= 1,
    x <= 2,
    is.na(x)
  )
  values <- list(
    1,
    2,
    0
  )

  expect_identical(
    vec_case_when(conditions, values),
    c(1, 2, NA, 0)
  )
})

test_that("Unused logical `NA` can still be cast to `values` ptype", {
  # Requires that casting happen before recycling, because it recycles
  # to size zero, resulting in a logical rather than an unspecified.
  expect_identical(vec_case_when(list(TRUE, FALSE), list("x", NA)), "x")
  expect_identical(vec_case_when(list(FALSE, TRUE), list("x", NA)), NA_character_)
})

test_that("`conditions` inputs can be size zero", {
  expect_identical(
    vec_case_when(
      list(logical(), logical()),
      list(1, 2)
    ),
    numeric()
  )

  expect_snapshot(error = TRUE, {
    vec_case_when(list(logical()), list(1:2))
  })
})

test_that("retains names of `values` inputs", {
  value1 <- c(x = 1, y = 2)
  value2 <- c(z = 3, w = 4)

  out <- vec_case_when(
    list(c(TRUE, FALSE), c(TRUE, TRUE)),
    list(value1, value2)
  )

  expect_named(out, c("x", "w"))
})

test_that("`values` are cast to their common type", {
  expect_identical(vec_case_when(list(FALSE, TRUE), list(1, 2L)), 2)
  expect_identical(vec_case_when(list(FALSE, TRUE), list(1, NA)), NA_real_)

  expect_snapshot(error = TRUE, {
    vec_case_when(list(FALSE, TRUE), list(1, "x"))
  })
})

test_that("`values` must be size 1 or same size as the `conditions`", {
  expect_identical(
    vec_case_when(
      list(c(TRUE, TRUE)),
      list(1)
    ),
    c(1, 1)
  )
  expect_identical(
    vec_case_when(
      list(c(TRUE, FALSE), c(TRUE, TRUE)),
      list(c(1, 2), c(3, 4))
    ),
    c(1, 4)
  )

  expect_snapshot(error = TRUE, {
    vec_case_when(
      list(c(TRUE, FALSE, TRUE, TRUE)),
      list(1:3)
    )
  })
})

test_that("Unhandled `NA` are given a value of `default`", {
  expect_identical(
    vec_case_when(list(NA), list(1)),
    NA_real_
  )

  expect_identical(
    vec_case_when(list(NA), list(1), default = 2),
    2
  )

  expect_identical(
    vec_case_when(
      list(
        c(FALSE, NA, TRUE, FALSE),
        c(NA, FALSE, TRUE, FALSE)
      ),
      list(
        2,
        3
      ),
      default = 4
    ),
    c(4, 4, 2, 4)
  )
})

test_that("`NA` is overridden by any `TRUE` values", {
  x <- c(1, 2, NA, 3)
  expect <- c("one", "not_one", "missing", "not_one")

  # `TRUE` overriding before the `NA`
  conditions <- list(
    is.na(x),
    x == 1
  )
  values <- list(
    "missing",
    "one"
  )
  expect_identical(
    vec_case_when(
      conditions,
      values,
      default = "not_one"
    ),
    expect
  )

  # `TRUE` overriding after the `NA`
  conditions <- list(
    x == 1,
    is.na(x)
  )
  values <- list(
    "one",
    "missing"
  )
  expect_identical(
    vec_case_when(
      conditions,
      values,
      default = "not_one"
    ),
    expect
  )
})

test_that("works when there is a used `default` and no missing values", {
  expect_identical(vec_case_when(list(c(TRUE, FALSE)), list(1), default = 3:4), c(1, 4))
})

test_that("works when there are missing values but no `default`", {
  expect_identical(vec_case_when(list(c(TRUE, NA)), list(1)), c(1, NA))
})

test_that("A `NULL` `default` fills in with missing values", {
  expect_identical(
    vec_case_when(list(c(TRUE, FALSE, FALSE)), list(1)),
    c(1, NA, NA)
  )
})

test_that("`default` fills in all unused slots", {
  expect_identical(
    vec_case_when(list(c(TRUE, FALSE, FALSE)), list(1), default = 2),
    c(1, 2, 2)
  )
})

test_that("`default` is initialized correctly in the logical / unspecified case", {
  # i.e. `vec_ptype(NA)` is unspecified but the result should be finalized to logical
  expect_identical(vec_case_when(list(FALSE), list(NA)), NA)
})

test_that("`default` can be vectorized, and is sliced to fit as needed", {
  out <- vec_case_when(
    list(
      c(FALSE, TRUE, FALSE, TRUE, FALSE),
      c(FALSE, TRUE, FALSE, FALSE, TRUE)
    ),
    list(
      1:5,
      6:10
    ),
    default = 11:15
  )

  expect_identical(out, c(11L, 2L, 13L, 4L, 10L))
})

test_that("`default` must be size 1 or same size as `conditions` (exact same as any other `values` input)", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(FALSE), list(1L), default = 2:3)
  })
})

test_that("`default` participates in common type determination (exact same as any other `values` input)", {
  expect_identical(vec_case_when(list(FALSE), list(1L), default = 2), 2)
})

test_that("`default` that is an unused logical `NA` can still be cast to `values` ptype", {
  # Requires that casting happen before recycling, because it recycles
  # to size zero, resulting in a logical rather than an unspecified.
  expect_identical(vec_case_when(list(TRUE), list("x"), default = NA), "x")
})

test_that("`default_arg` can be customized", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(FALSE), list(1L), default = 2:3, default_arg = "foo")
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(FALSE), list(1L), default = "x", default_arg = "foo")
  })
})

test_that("`conditions_arg` is validated", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(1), conditions_arg = 1)
  })
})

test_that("`values_arg` is validated", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(1), values_arg = 1)
  })
})

test_that("`default_arg` is validated", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(1), default_arg = 1)
  })
})

test_that("`conditions` must all be the same size", {
  expect_snapshot(error = TRUE, {
    vec_case_when(
      list(c(TRUE, FALSE), TRUE),
      list(1, 2)
    )
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(
      list(c(TRUE, FALSE), c(TRUE, FALSE, TRUE)),
      list(1, 2)
    )
  })
})

test_that("`conditions` must be logical (and aren't cast to logical!)", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(1), list(2))
  })

  # Make sure input numbering is right in the error message!
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE, 3.5), list(2, 4))
  })
})

test_that("`size` overrides the `conditions` sizes", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(1), size = 5)
  })

  expect_snapshot(error = TRUE, {
    vec_case_when(
      list(c(TRUE, FALSE), c(TRUE, FALSE, TRUE)),
      list(1, 2),
      size = 2
    )
  })
})

test_that("`ptype` overrides the `values` types", {
  expect_identical(
    vec_case_when(list(FALSE, TRUE), list(1, 2), ptype = integer()),
    2L
  )

  expect_snapshot(error = TRUE, {
    vec_case_when(list(FALSE, TRUE), list(1, 2), ptype = character())
  })
})

test_that("number of `conditions` and `values` must be the same", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list())
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE, TRUE), list(1))
  })
})

test_that("can't have empty inputs", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(), list())
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(), list(), default = 1)
  })
})

test_that("dots must be empty", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(1), 2)
  })
})

test_that("`conditions` must be a list", {
  expect_snapshot(error = TRUE, {
    vec_case_when(1, list(2))
  })
})

test_that("`values` must be a list", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), 1)
  })
})

test_that("named inputs show up in the error message", {
  expect_snapshot(error = TRUE, {
    vec_case_when(list(x = 1.5), list(1))
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(x = 1.5), list(1), conditions_arg = "foo")
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(x = 1.5), list(1), conditions_arg = "")
  })

  expect_snapshot(error = TRUE, {
    vec_case_when(list(x = TRUE, y = c(TRUE, FALSE)), list(1, 2))
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(x = TRUE, y = c(TRUE, FALSE)), list(1, 2), conditions_arg = "foo")
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(x = TRUE, y = c(TRUE, FALSE)), list(1, 2), conditions_arg = "")
  })

  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE, FALSE), list(1, x = "y"))
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE, FALSE), list(1, x = "y"), values_arg = "foo")
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE, FALSE), list(1, x = "y"), values_arg = "")
  })

  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(NULL))
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(x = NULL))
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(NULL), values_arg = "foo")
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(x = NULL), values_arg = "foo")
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(NULL), values_arg = "")
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(list(TRUE), list(x = NULL), values_arg = "")
  })
})
