test_that("works like a vectorized switch", {
  out <- vec_case_match(
    needles = c(1, 4, 2, 1),
    haystacks = list(1, 2, 4),
    values = list("a", "b", "d")
  )

  expect_identical(out, c("a", "d", "b", "a"))
})

test_that("the first match in `haystacks` is always used", {
  out <- vec_case_match(
    needles = c(1, 4, 2, 1),
    haystacks = list(1, 2, 1, 4, 2),
    values = list("a", "b", "c", "d", "e")
  )

  expect_identical(out, c("a", "d", "b", "a"))
})

test_that("`haystacks` can contain multiple values", {
  out <- vec_case_match(
    needles = c(1, 4, 2, 1),
    haystacks = list(c(1, 2), c(4, 5)),
    values = list("a", "b")
  )

  expect_identical(out, c("a", "b", "a", "a"))
})

test_that("`values` can be vectorized on the size of `needles`", {
  out <- vec_case_match(
    needles = c(1, 4, 2, 1),
    haystacks = list(c(1, 2), c(4, 5)),
    values = list(1:4, 5:8)
  )

  expect_identical(out, c(1L, 6L, 3L, 4L))
})

test_that("unmatched value falls through to `default`", {
  out <- vec_case_match(
    needles = c(1, 4, 2, 1, 5),
    haystacks = list(1, 2),
    values = list("a", "b")
  )

  expect_identical(out, c("a", NA, "b", "a", NA))

  out <- vec_case_match(
    needles = c(1, 4, 2, 1, 5),
    haystacks = list(1, 2),
    values = list("a", "b"),
    default = "na"
  )

  expect_identical(out, c("a", "na", "b", "a", "na"))
})

test_that("`default` can be vectorized on the size of `needles`", {
  out <- vec_case_match(
    needles = c(1, 4, 2, 1, 5),
    haystacks = list(1, 2),
    values = list("a", "b"),
    default = c("one", "two", "three", "four", "five")
  )

  expect_identical(out, c("a", "two", "b", "a", "five"))
})

test_that("unmatched missing values get `default`", {
  out <- vec_case_match(
    needles = c(1, 4, 2, NA, NA),
    haystacks = list(1, 2),
    values = list("a", "b")
  )

  expect_identical(out, c("a", NA, "b", NA, NA))

  out <- vec_case_match(
    needles = c(1, 4, 2, NA, NA),
    haystacks = list(1, 2),
    values = list("a", "b"),
    default = "na"
  )

  expect_identical(out, c("a", "na", "b", "na", "na"))
})

test_that("can exactly match on missing values", {
  out <- vec_case_match(
    needles = c(NA, NaN, NA),
    haystacks = list(NA, NaN),
    values = list("na", "nan")
  )

  expect_identical(out, c("na", "nan", "na"))
})

test_that("`haystacks` must be castable to `needles`", {
  expect_snapshot(error = TRUE, {
    vec_case_match(1L, haystacks = list(1.5), values = list(2))
  })
})

test_that("`ptype` overrides `values` common type", {
  expect_identical(
    vec_case_match(
      1:2,
      haystacks = list(1),
      values = list(0),
      ptype = integer()
    ),
    c(0L, NA)
  )

  expect_snapshot(error = TRUE, {
    vec_case_match(
      1:2,
      haystacks = list(1),
      values = list(1.5),
      ptype = integer()
    )
  })
})

test_that("`default` is considered in the common type computation", {
  expect_identical(
    vec_case_match(1, haystacks = list(1), values = list(2L), default = 1.5),
    2
  )
})

test_that("`default` respects `ptype`", {
  expect_identical(
    vec_case_match(
      needles = 1,
      haystacks = list(1),
      values = list(2L),
      default = 1,
      ptype = integer()
    ),
    2L
  )

  expect_snapshot(error = TRUE, {
    vec_case_match(
      needles = 1,
      haystacks = list(1),
      values = list(2L),
      default = 1.5,
      ptype = integer()
    )
  })
})

test_that("`NULL` values in `haystacks` and `values` are not dropped", {
  expect_snapshot(error = TRUE, {
    vec_case_match(1:2, list(1, NULL, 2), list("a", NULL, "b"))
  })
  expect_snapshot(error = TRUE, {
    vec_case_match(1:2, list(1, NULL, 2), list("a", "a", "b"))
  })
  expect_snapshot(error = TRUE, {
    vec_case_match(1:2, list(1, 1, 2), list("a", NULL, "b"))
  })
})

test_that("size of `needles` is maintained", {
  expect_snapshot(error = TRUE, {
    vec_case_match(1, haystacks = list(1), values = list(1:2))
  })
})

test_that("requires at least one condition", {
  expect_snapshot(error = TRUE, {
    vec_case_match(1, haystacks = list(), values = list())
  })
})

test_that("input must be a vector", {
  expect_snapshot(error = TRUE, {
    vec_case_match(
      environment(),
      haystacks = list(environment()),
      values = list(1)
    )
  })
})

test_that("`haystacks` must be a list", {
  expect_snapshot(error = TRUE, {
    vec_case_match(1, haystacks = 1, values = list(2))
  })
})

test_that("`values` must be a list", {
  expect_snapshot(error = TRUE, {
    vec_case_match(1, haystacks = list(1), values = 2)
  })
})

test_that("`needles_arg` is respected", {
  expect_snapshot(error = TRUE, {
    vec_case_match(
      needles = environment(),
      haystacks = list(environment()),
      values = list(1),
      needles_arg = "foo"
    )
  })

  expect_snapshot(error = TRUE, {
    vec_case_match(
      needles = environment(),
      haystacks = list(environment()),
      values = list(1),
      needles_arg = ""
    )
  })
})

test_that("`haystacks_arg` is respected", {
  expect_snapshot(error = TRUE, {
    vec_case_match(
      needles = 1,
      haystacks = 1,
      values = list(1),
      haystacks_arg = "foo"
    )
  })
  expect_snapshot(error = TRUE, {
    vec_case_match(
      needles = 1,
      haystacks = 1,
      values = list(1),
      haystacks_arg = ""
    )
  })

  expect_snapshot(error = TRUE, {
    vec_case_match(
      needles = 1,
      haystacks = list(a = "x"),
      values = list(1),
      haystacks_arg = "foo"
    )
  })
  expect_snapshot(error = TRUE, {
    vec_case_match(
      needles = 1,
      haystacks = list("x"),
      values = list(1),
      haystacks_arg = ""
    )
  })
})

test_that("`values_arg` is respected", {
  expect_snapshot(error = TRUE, {
    vec_case_match(
      needles = 1,
      haystacks = list(1, 2),
      values = list("x", b = 1),
      values_arg = "foo"
    )
  })

  expect_snapshot(error = TRUE, {
    vec_case_match(
      needles = 1,
      haystacks = list(1, 2),
      values = list("x", b = 1),
      values_arg = ""
    )
  })
})

test_that("`default_arg` is respected", {
  expect_snapshot(error = TRUE, {
    vec_case_match(
      needles = 1,
      haystacks = list(1),
      values = list(2L),
      default = 1.5,
      default_arg = "foo",
      ptype = integer()
    )
  })

  expect_snapshot(error = TRUE, {
    vec_case_match(
      needles = 1,
      haystacks = list(1),
      values = list(2L),
      default = 1.5,
      default_arg = "",
      ptype = integer()
    )
  })
})
