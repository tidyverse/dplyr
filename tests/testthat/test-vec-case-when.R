test_that("works with data frames", {
  out <- vec_case_when(
    c(FALSE, TRUE, FALSE, FALSE), vctrs::data_frame(x = 1, y = 2),
    c(TRUE, TRUE, FALSE, FALSE), vctrs::data_frame(x = 3, y = 4),
    c(FALSE, TRUE, FALSE, TRUE), vctrs::data_frame(x = 3:6, y = 4:7),
  )

  expect_identical(
    out,
    vctrs::data_frame(
      x = c(3, 1, NA, 6),
      y = c(4, 2, NA, 7)
    )
  )
})

test_that("first `TRUE` case wins", {
  expect_identical(
    vec_case_when(c(TRUE, FALSE), 1, c(TRUE, TRUE), 2, c(TRUE, TRUE), 3),
    c(1, 2)
  )
})

test_that("can replace missing values by catching with `is.na()`", {
  x <- c(1:3, NA)

  expect_identical(
    vec_case_when(
      x <= 1, 1,
      x <= 2, 2,
      is.na(x), 0
    ),
    c(1, 2, NA, 0)
  )
})

test_that("Unused logical `NA` can still be cast to `...` ptype", {
  # Requires that casting happen before recycling, because it recycles
  # to size zero, resulting in a logical rather than an unspecified.
  expect_identical(vec_case_when(TRUE, "x", FALSE, NA), "x")
  expect_identical(vec_case_when(FALSE, "x", TRUE, NA), NA_character_)
})

test_that("odd numbered inputs can be size zero", {
  expect_identical(
    vec_case_when(
      logical(), 1,
      logical(), 2
    ),
    numeric()
  )

  expect_snapshot(error = TRUE, {
    vec_case_when(logical(), 1:2)
  })
})

test_that("retains names of inputs", {
  value1 <- c(x = 1, y = 2)
  value2 <- c(z = 3, w = 4)

  out <- vec_case_when(
    c(TRUE, FALSE), value1,
    c(TRUE, TRUE), value2
  )

  expect_named(out, c("x", "w"))
})

test_that("even numbered inputs are cast to their common type", {
  expect_identical(vec_case_when(FALSE, 1, TRUE, 2L), 2)
  expect_identical(vec_case_when(FALSE, 1, TRUE, NA), NA_real_)

  expect_snapshot(error = TRUE, {
    vec_case_when(FALSE, 1, TRUE, "x")
  })
})

test_that("even numbered inputs must be size 1 or same size as logical conditions", {
  expect_identical(
    vec_case_when(c(TRUE, TRUE), 1),
    c(1, 1)
  )
  expect_identical(
    vec_case_when(c(TRUE, FALSE), c(1, 2), c(TRUE, TRUE), c(3, 4)),
    c(1, 4)
  )

  # Make sure input numbering is right in the error message!
  expect_snapshot(error = TRUE, {
    vec_case_when(c(TRUE, FALSE, TRUE, TRUE), 1:3)
  })
})

test_that("Unhandled `NA` are given a value of `.missing`", {
  expect_identical(
    vec_case_when(NA, 1, .default = 2),
    NA_real_
  )

  expect_identical(
    vec_case_when(NA, 1, .default = 2, .missing = 3),
    3
  )

  expect_identical(
    vec_case_when(
      c(FALSE, NA, TRUE, FALSE),
      2,
      c(NA, FALSE, TRUE, FALSE),
      3,
      .default = 4
    ),
    c(NA, NA, 2, 4)
  )

  expect_identical(
    vec_case_when(
      c(FALSE, NA, TRUE, FALSE),
      2,
      c(NA, FALSE, TRUE, FALSE),
      3,
      .default = 4,
      .missing = 5
    ),
    c(5, 5, 2, 4)
  )
})

test_that("`NA` is overridden by any `TRUE` values", {
  x <- c(1, 2, NA, 3)
  expect <- c("one", "not_one", "missing", "not_one")

  # `TRUE` overriding before the `NA`
  expect_identical(
    vec_case_when(
      is.na(x), "missing",
      x == 1, "one",
      .default = "not_one"
    ),
    expect
  )

  # `TRUE` overriding after the `NA`
  expect_identical(
    vec_case_when(
      x == 1, "one",
      is.na(x), "missing",
      .default = "not_one"
    ),
    expect
  )
})

test_that("works when there is a used `.default` and no missing values", {
  expect_identical(vec_case_when(c(TRUE, FALSE), 1, .default = 3:4), c(1, 4))
})

test_that("works when there are missing values but no `.default`", {
  expect_identical(vec_case_when(c(TRUE, NA), 1), c(1, NA))
})

test_that("A `NULL` `.default` fills in with missing values", {
  expect_identical(
    vec_case_when(c(TRUE, FALSE, FALSE), 1),
    c(1, NA, NA)
  )
})

test_that("A `NULL` `.missing` fills in with missing values", {
  expect_identical(
    vec_case_when(c(TRUE, NA, NA), 1),
    c(1, NA, NA)
  )
})

test_that("`.default` fills in all unused slots", {
  expect_identical(
    vec_case_when(c(TRUE, FALSE, FALSE), 1, .default = 2),
    c(1, 2, 2)
  )
})

test_that("`.default` is initialized correctly in the logical / unspecified case", {
  # i.e. `vec_ptype(NA)` is unspecified but the result should be finalized to logical
  expect_identical(vec_case_when(FALSE, NA), NA)
})

test_that("`.default` can be vectorized, and is sliced to fit as needed", {
  out <- vec_case_when(
    c(FALSE, TRUE, FALSE, TRUE, FALSE), 1:5,
    c(FALSE, TRUE, FALSE, FALSE, TRUE), 6:10,
    .default = 11:15
  )

  expect_identical(out, c(11L, 2L, 13L, 4L, 10L))
})

test_that("`.missing` can be vectorized, and is sliced to fit as needed", {
  out <- vec_case_when(
    c(NA, NA, FALSE, TRUE, FALSE), 1:5,
    c(FALSE, TRUE, NA, FALSE, TRUE), 6:10,
    .missing = 11:15
  )

  expect_identical(out, c(11L, 7L, 13L, 4L, 10L))
})

test_that("`.default` must be size 1 or same size as logical conditions (exact same as any other even numbered input)", {
  expect_snapshot(error = TRUE, {
    vec_case_when(FALSE, 1L, .default = 2:3)
  })
})

test_that("`.missing` must be size 1 or same size as logical conditions (exact same as any other even numbered input)", {
  expect_snapshot(error = TRUE, {
    vec_case_when(FALSE, 1L, .missing = 2:3)
  })
})

test_that("`.default` participates in common type determination (exact same as any other even numbered input)", {
  expect_identical(vec_case_when(FALSE, 1L, .default = 2), 2)
})

test_that("`.missing` participates in common type determination (exact same as any other even numbered input)", {
  expect_identical(vec_case_when(NA, 1L, .missing = 2), 2)
})

test_that("`.default` that is an unused logical `NA` can still be cast to `...` ptype", {
  # Requires that casting happen before recycling, because it recycles
  # to size zero, resulting in a logical rather than an unspecified.
  expect_identical(vec_case_when(TRUE, "x", .default = NA), "x")
})

test_that("`.missing` that is an unused logical `NA` can still be cast to `...` ptype", {
  # Requires that casting happen before recycling, because it recycles
  # to size zero, resulting in a logical rather than an unspecified.
  expect_identical(vec_case_when(TRUE, "x", .missing = NA), "x")
})

test_that("`.default_arg` can be customized", {
  expect_snapshot(error = TRUE, {
    vec_case_when(FALSE, 1L, .default = 2:3, .default_arg = "foo")
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(FALSE, 1L, .default = "x", .default_arg = "foo")
  })
})

test_that("`.missing_arg` can be customized", {
  expect_snapshot(error = TRUE, {
    vec_case_when(FALSE, 1L, .missing = 2:3, .missing_arg = "foo")
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(FALSE, 1L, .missing = "x", .missing_arg = "foo")
  })
})

test_that("`.default_arg` is validated", {
  expect_snapshot(error = TRUE, {
    vec_case_when(TRUE, 1, .default_arg = 1)
  })
})

test_that("`.missing_arg` is validated", {
  expect_snapshot(error = TRUE, {
    vec_case_when(TRUE, 1, .missing_arg = 1)
  })
})

test_that("odd numbered inputs must all be the same size", {
  expect_snapshot(error = TRUE, {
    vec_case_when(c(TRUE, FALSE), 1, TRUE, 2)
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(c(TRUE, FALSE), 1, c(TRUE, FALSE, TRUE), 2)
  })
})

test_that("odd numbered inputs must be logical (and aren't cast to logical!)", {
  expect_snapshot(error = TRUE, {
    vec_case_when(1, 2)
  })

  # Make sure input numbering is right in the error message!
  expect_snapshot(error = TRUE, {
    vec_case_when(TRUE, 2, 3.5, 4)
  })
})

test_that("`.size` overrides the odd numbered input sizes", {
  expect_snapshot(error = TRUE, {
    vec_case_when(TRUE, 1, .size = 5)
  })

  # Make sure input numbering is right in the error message!
  expect_snapshot(error = TRUE, {
    vec_case_when(c(TRUE, FALSE), 1, c(TRUE, FALSE, TRUE), 2, .size = 2)
  })
})

test_that("`.ptype` overrides the even numbered input types", {
  expect_identical(
    vec_case_when(FALSE, 1, TRUE, 2, .ptype = integer()),
    2L
  )

  # Make sure input numbering is right in the error message!
  expect_snapshot(error = TRUE, {
    vec_case_when(FALSE, 1, TRUE, 2, .ptype = character())
  })
})

test_that("can't have an odd number of inputs", {
  expect_snapshot(error = TRUE, {
    vec_case_when(1)
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(1, 2, 3)
  })
})

test_that("can't have empty dots", {
  expect_snapshot(error = TRUE, {
    vec_case_when()
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(.default = 1)
  })
})

test_that("named dots show up in the error message", {
  expect_snapshot(error = TRUE, {
    vec_case_when(x = 1.5, 1)
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(x = TRUE, 1, y = c(TRUE, FALSE), 2)
  })
  expect_snapshot(error = TRUE, {
    vec_case_when(TRUE, 1, FALSE, x = "y")
  })
})
