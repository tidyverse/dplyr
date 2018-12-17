context("combine")

test_that("combine handles NULL (#1596, #3365)", {
  expect_equal(combine(list(NULL, 1, 2)), c(1, 2))
  expect_equal(combine(list(1, NULL, 2)), c(1, 2))
  expect_equal(combine(list(1, 2, NULL)), c(1, 2))
  expect_equal(combine(), logical())
  expect_equal(combine(list(NULL)), logical())
  expect_equal(combine(list(NULL, NULL), list(NULL)), list(NULL, NULL, NULL))
  expect_equal(combine(NULL, list(NULL, NULL)), list(NULL, NULL))
})

test_that("combine complains about incompatibilites", {
  expect_error(
    combine("a", 1),
    "Argument 2 can't be converted from numeric to character"
  )
  expect_error(
    combine(factor("a"), 1L),
    "Argument 2 can't be converted from integer to factor"
  )
})

test_that("combine works with input that used to fail (#1780)", {
  no <- list(alpha = letters[1:3], omega = letters[24:26])
  expect_equal(combine(no), unlist(no, use.names = FALSE))
})

test_that("combine works with NA and logical (#2203)", {
  # NA first
  expected_result <- c(NA, TRUE, FALSE, NA, TRUE)
  works1 <- combine(list(NA, TRUE, FALSE, NA, TRUE))
  expect_equal(works1, expected_result)

  # NA length == 1
  expected_result <- c(TRUE, FALSE, NA, TRUE)
  works1 <- combine(list(TRUE, FALSE, NA, TRUE))
  expect_equal(works1, expected_result)

  # NA length > 1
  expected_result <- c(TRUE, FALSE, NA, NA, TRUE)
  works3 <- combine(list(TRUE, FALSE, c(NA, NA), TRUE))
  expect_equal(works3, expected_result)
})

test_that("combine works with NA and integers (#2203)", {
  works <- combine(list(1L, 2L, NA, 4L))
  expect_equal(works, c(1L, 2L, NA, 4L))
  works <- combine(list(1L, 2L, c(NA, NA), 4L))
  expect_equal(works, c(1L, 2L, NA, NA, 4L))
})

test_that("combine works with NA and factors (#2203)", {
  # NA first
  fac <- factor(c("a", "c", NA, "b"), levels = letters[1:3])
  expected_result <- fac[c(3, 1, 3, 2)]
  works1 <- combine(list(NA, fac[1], NA, fac[2]))
  expect_equal(works1, expected_result)

  # NA length == 1
  expected_result <- fac
  works1 <- combine(list(fac[1], fac[2], fac[3], fac[4]))
  expect_equal(works1, expected_result)

  works2 <- combine(list(fac[1], fac[2], NA, fac[4]))
  expect_equal(works2, expected_result)

  # NA length > 1
  expected_result <- fac[c(1, 2, 3, 3, 4)]

  works3 <- combine(list(fac[1], fac[2], fac[c(3, 3)], fac[4]))
  expect_equal(works3, expected_result)

  works4 <- combine(list(fac[1], fac[2], c(NA, NA), fac[4]))
  expect_equal(works4, expected_result)
})

test_that("combine works with NA and double (#2203)", {
  # NA first
  works <- combine(list(NA, 1.5, 2.5, NA, 4.5))
  expect_equal(works, c(NA, 1.5, 2.5, NA, 4.5))
  # NA length 1
  works <- combine(list(1.5, 2.5, NA, 4.5))
  expect_equal(works, c(1.5, 2.5, NA, 4.5))
  # NA length > 1
  works <- combine(list(1.5, 2.5, c(NA, NA), 4.5))
  expect_equal(works, c(1.5, 2.5, NA, NA, 4.5))
})

test_that("combine works with NA and characters (#2203)", {
  # NA first
  works <- combine(list(NA, "a", "b", "c", NA, "e"))
  expect_equal(works, c(NA, "a", "b", "c", NA, "e"))
  # NA length 1
  works <- combine(list("a", "b", "c", NA, "e"))
  expect_equal(works, c("a", "b", "c", NA, "e"))
  # NA length > 1
  works <- combine(list("a", "b", "c", c(NA, NA), "e"))
  expect_equal(works, c("a", "b", "c", NA, NA, "e"))
})


test_that("combine works with NA and POSIXct (#2203)", {
  # NA first
  works <- combine(list(
    NA, as.POSIXct("2010-01-01"), as.POSIXct("2010-01-02"),
    NA, as.POSIXct("2010-01-04")
  ))
  expect_equal(works, c(as.POSIXct(c(
    NA, "2010-01-01", "2010-01-02",
    NA, "2010-01-04"
  ))))
  # NA length 1
  works <- combine(list(
    as.POSIXct("2010-01-01"), as.POSIXct("2010-01-02"),
    NA, as.POSIXct("2010-01-04")
  ))
  expect_equal(works, c(as.POSIXct(c(
    "2010-01-01", "2010-01-02",
    NA, "2010-01-04"
  ))))
  # NA length > 1
  works <- combine(list(
    as.POSIXct("2010-01-01"), as.POSIXct("2010-01-02"),
    c(NA, NA), as.POSIXct("2010-01-04")
  ))
  expect_equal(works, c(as.POSIXct(c(
    "2010-01-01", "2010-01-02",
    NA, NA, "2010-01-04"
  ))))
})

test_that("combine works with NA and Date (#2203)", {
  # NA first
  expected_result <- as.Date("2010-01-01") + c(NA, 1, 2, NA, 4)
  expect_equal(combine(as.list(expected_result)), expected_result)

  # NA length == 1
  expected_result <- c(as.Date(c("2010-01-01", "2010-01-02", NA, "2010-01-04")))
  works1 <- combine(list(
    as.Date("2010-01-01"), as.Date("2010-01-02"),
    as.Date(NA), as.Date("2010-01-04")
  ))
  expect_equal(works1, expected_result)

  works2 <- combine(list(
    as.Date("2010-01-01"), as.Date("2010-01-02"),
    NA, as.Date("2010-01-04")
  ))
  expect_equal(works2, expected_result)

  # NA length > 1
  expected_result <- as.Date("2010-01-01") + c(0, 1, NA, NA, 3)
  works1 <- combine(split(expected_result, c(1, 2, 3, 3, 4)))
  expect_equal(works1, expected_result)

  works2 <- combine(list(
    as.Date("2010-01-01"), as.Date("2010-01-02"),
    c(NA, NA),
    as.Date("2010-01-04")
  ))
  expect_equal(works2, expected_result)
})


test_that("combine works with NA and complex (#2203)", {
  # NA first
  expected_result <- c(NA, 1 + 2i)
  works1 <- combine(list(NA, 1 + 2i))
  expect_equal(works1, expected_result)

  # NA length == 1
  expected_result <- c(1, 2, NA, 4) + 1i

  expect_equal(combine(as.list(expected_result)), expected_result)

  works2 <- combine(list(1 + 1i, 2 + 1i, NA, 4 + 1i))
  expect_equal(works2, expected_result)

  # NA length > 1
  expected_result <- c(1, 2, NA, NA, 4) + 1i
  expect_equal(
    combine(split(expected_result, c(1, 2, 3, 3, 4))),
    expected_result
  )

  works3 <- combine(list(1 + 1i, 2 + 1i, c(NA, NA), 4 + 1i))
  expect_equal(works3, expected_result)
})

test_that("combine works with integer64 (#1092)", {
  expect_equal(
    combine(bit64::as.integer64(2^34), bit64::as.integer64(2^35)),
    bit64::as.integer64(c(2^34, 2^35))
  )
})

test_that("combine works with difftime", {
  expect_equal(
    combine(as.difftime(1, units = "mins"), as.difftime(1, units = "hours")),
    as.difftime(c(60, 3600), units = "secs")
  )
  expect_equal(
    combine(as.difftime(1, units = "secs"), as.difftime(1, units = "secs")),
    as.difftime(c(1, 1), units = "secs")
  )
  expect_equal(
    combine(as.difftime(1, units = "days"), as.difftime(1, units = "secs")),
    as.difftime(c(24 * 60 * 60, 1), units = "secs")
  )
  expect_equal(
    combine(as.difftime(2, units = "weeks"), as.difftime(1, units = "secs")),
    as.difftime(c(2 * 7 * 24 * 60 * 60, 1), units = "secs")
  )
  expect_equal(
    combine(as.difftime(2, units = "weeks"), as.difftime(3, units = "weeks")),
    as.difftime(c(2, 3), units = "weeks")
  )
})

test_that("combine works with hms and difftime", {
  expect_equal(
    combine(as.difftime(2, units = "weeks"), hms::hms(hours = 1)),
    as.difftime(c(2 * 7 * 24 * 60 * 60, 3600), units = "secs")
  )
  expect_equal(
    combine(hms::hms(hours = 1), as.difftime(2, units = "weeks")),
    hms::hms(seconds = c(3600, 2 * 7 * 24 * 60 * 60))
  )
})

test_that("combine uses tidy dots (#3407)", {
  chunks <- list(1,2,3)
  expect_equal(combine(!!!chunks), c(1,2,3))
})

# Uses helper-combine.R
combine_coercion_types()
