context("hybrid")

test_that("n() and n_distinct() work", {
  check_hybrid_result(
    n(), a = 1:5,
    expected = 5L, test_eval = FALSE
  )
  check_not_hybrid_result(
    list(1:n()), a = 1:5,
    expected = list(1:5), test_eval = FALSE
  )

  check_hybrid_result(
    n_distinct(a), a = 1:5,
    expected = 5L
  )
  check_hybrid_result(
    n_distinct(a), a = rep(1L, 3),
    expected = 1L
  )
  check_hybrid_result(
    n_distinct(a, b), a = rep(1L, 3), b = 1:3,
    expected = 3L
  )
  check_hybrid_result(
    n_distinct(a, b), a = rep(1L, 3), b = c(1, 1, 2),
    expected = 2L
  )
  check_hybrid_result(
    n_distinct(a, b), a = rep(1L, 3), b = c(1, 1, NA),
    expected = 2L
  )
  check_hybrid_result(
    n_distinct(a, b, na.rm = TRUE), a = rep(1L, 3), b = c(1, 1, NA),
    expected = 1L
  )
  check_hybrid_result(
    n_distinct(a = a, b = b, na.rm = TRUE), a = rep(1L, 3), b = c(1, 1, NA),
    expected = 1L
  )

  expect_not_hybrid_error(
    n_distinct(), a = 1:5,
    error = "need at least one column"
  )

  skip("Currently failing (external var)")
  c <- 1:3
  check_not_hybrid_result(
    n_distinct(c), a = 1:5,
    expected = 3L, test_eval = FALSE
  )
  check_not_hybrid_result(
    n_distinct(a, c), a = 1:3,
    expected = 3L, test_eval = FALSE
  )
  check_not_hybrid_result(
    n_distinct(a, b, na.rm = 1), a = rep(1L, 3), b = c(1, 1, NA),
    expected = 1L
  )
})

test_that("%in% works (#192)", {
  # compilation errors on Windows
  # https://ci.appveyor.com/project/hadley/dplyr/build/1.0.230
  check_not_hybrid_result(
    list(a %in% (1:3 * 1i)), a = 2:4 * 1i,
    expected = list(c(TRUE, TRUE, FALSE))
  )

  check_not_hybrid_result(
    list(a %in% 1:3), a = as.numeric(2:4),
    expected = list(c(TRUE, TRUE, FALSE))
  )
  check_not_hybrid_result(
    list(a %in% as.numeric(1:3)), a = 2:4,
    expected = list(c(TRUE, TRUE, FALSE))
  )

  c <- 2:4
  check_not_hybrid_result(
    list(c %in% 1:3), a = as.numeric(2:4),
    expected = list(c(TRUE, TRUE, FALSE))
  )

  skip("Currently failing (constfold)")
  check_hybrid_result(
    list(a %in% 1:3), a = 2:4,
    expected = list(c(TRUE, TRUE, FALSE))
  )
  check_hybrid_result(
    list(a %in% as.numeric(1:3)), a = as.numeric(2:4),
    expected = list(c(TRUE, TRUE, FALSE))
  )
  check_hybrid_result(
    list(a %in% letters[1:3]), a = letters[2:4],
    expected = list(c(TRUE, TRUE, FALSE))
  )
  check_hybrid_result(
    list(a %in% c(TRUE, FALSE)), a = c(TRUE, FALSE, NA),
    expected = list(c(TRUE, TRUE, FALSE))
  )

  skip("Currently failing")
  check_hybrid_result(
    list(a %in% NA_integer_), a = c(2:4, NA),
    expected = list(c(FALSE, FALSE, FALSE, TRUE))
  )
  check_hybrid_result(
    list(a %in% NA_real_), a = as.numeric(c(2:4, NA)),
    expected = list(c(FALSE, FALSE, FALSE, TRUE))
  )
  check_hybrid_result(
    list(a %in% NA_character_), a = c(letters[2:4], NA),
    expected = list(c(FALSE, FALSE, FALSE, TRUE))
  )
  check_hybrid_result(
    list(a %in% NA), a = c(TRUE, FALSE, NA),
    expected = list(c(FALSE, FALSE, TRUE))
  )
})

test_that("min() and max() work", {
  check_hybrid_result(
    min(a), a = 1:5,
    expected = 1L
  )
  check_hybrid_result(
    max(a), a = 1:5,
    expected = 5L
  )
  check_hybrid_result(
    min(a), a = as.numeric(1:5),
    expected = 1
  )
  check_hybrid_result(
    max(a), a = as.numeric(1:5),
    expected = 5
  )
  check_hybrid_result(
    min(a), a = c(1:5, NA),
    expected = NA_integer_
  )
  check_hybrid_result(
    max(a), a = c(1:5, NA),
    expected = NA_integer_
  )

  c <- 1:3
  check_not_hybrid_result(
    min(c), a = 1:5,
    expected = 1L
  )
  check_not_hybrid_result(
    max(c), a = 1:5,
    expected = 3L
  )

  check_not_hybrid_result(
    min(a), a = letters,
    expected = "a"
  )
  check_not_hybrid_result(
    max(a), a = letters,
    expected = "z"
  )
  check_not_hybrid_result(
    min(a), a = c(letters, NA),
    expected = NA_character_
  )
  check_not_hybrid_result(
    max(a), a = c(letters, NA),
    expected = NA_character_
  )
  check_not_hybrid_result(
    min(a, na.rm = TRUE), a = c(letters, NA),
    expected = "a"
  )
  check_not_hybrid_result(
    max(a, na.rm = TRUE), a = c(letters, NA),
    expected = "z"
  )

  skip("Currently failing (constfold)")
  check_hybrid_result(
    min(a, na.rm = (1 == 0)), a = c(1:5, NA),
    expected = NA_integer_
  )
  check_hybrid_result(
    max(a, na.rm = (1 == 0)), a = c(1:5, NA),
    expected = NA_integer_
  )
  check_hybrid_result(
    min(a, na.rm = (1 == 1)), a = c(1:5, NA),
    expected = 1L
  )
  check_hybrid_result(
    max(a, na.rm = (1 == 1)), a = c(1:5, NA),
    expected = 5L
  )

  check_hybrid_result(
    min(a, na.rm = pi != pi), a = c(1:5, NA),
    expected = NA_integer_
  )
  check_hybrid_result(
    max(a, na.rm = pi != pi), a = c(1:5, NA),
    expected = NA_integer_
  )
  check_hybrid_result(
    min(a, na.rm = pi == pi), a = c(1:5, NA),
    expected = 1L
  )
  check_hybrid_result(
    max(a, na.rm = pi == pi), a = c(1:5, NA),
    expected = 5L
  )

  skip("Currently failing")
  check_hybrid_result(
    min(a, na.rm = F), a = c(1:5, NA),
    expected = NA_integer_
  )
  check_hybrid_result(
    max(a, na.rm = F), a = c(1:5, NA),
    expected = NA_integer_
  )
  check_hybrid_result(
    min(a, na.rm = T), a = c(1:5, NA),
    expected = 1L
  )
  check_hybrid_result(
    max(a, na.rm = T), a = c(1:5, NA),
    expected = 5L
  )

  skip("Currently failing (#2305)")
  check_hybrid_result(
    min(a, na.rm = TRUE), a = NA_real_,
    expected = Inf
  )
  check_hybrid_result(
    max(a, na.rm = TRUE), a = NA_integer_,
    expected = -Inf
  )
})

test_that("first(), last(), and nth() work", {
  check_hybrid_result(
    first(a), a = 1:5,
    expected = 1L
  )
  check_hybrid_result(
    last(a), a = as.numeric(1:5),
    expected = 5
  )
  check_hybrid_result(
    nth(a, 6, default = 3L), a = as.numeric(1:5),
    expected = 3
  )
  check_hybrid_result(
    nth(a, 6, def = 3L), a = as.numeric(1:5),
    expected = 3
  )
  check_hybrid_result(
    nth(a, 6.5), a = 1:5,
    expected = NA_integer_
  )

  check_not_hybrid_result(
    nth(a, b[[2]]), a = letters[1:5], b = 5:1,
    expected = "d"
  )

  skip("Currently failing (data types)")
  check_hybrid_result(
    nth(a, 3), a = as.numeric(1:5) * 1i,
    expected = 3i
  )
  check_hybrid_result(
    nth(a, 1 + 2), a = letters[1:5],
    expected = "c"
  )
  check_not_hybrid_result(
    nth(a, 2), a = as.list(1:5),
    expected = 2L
  )

  skip("Currently failing (negative value)")
  check_hybrid_result(
    nth(a, -4), a = 1:5,
    expected = 2L
  )

  skip("Currently failing (match call)")
  check_not_hybrid_result(
    nth(a, order_by = 5:1, 2), a = 1:5,
    expected = 4L
  )
  expect_not_hybrid_error(
    first(a, bogus = 3), a = 1:5,
    error = "unused argument"
  )
  expect_not_hybrid_error(
    last(a, bogus = 3), a = 1:5,
    error = "unused argument"
  )
  expect_not_hybrid_error(
    nth(a, 3, bogus = 3), a = 1:5,
    error = "unused argument"
  )

  skip("Currently failing (external variable)")
  c <- 1:3
  check_not_hybrid_result(
    first(c), a = 2:4,
    expected = 1L
  )
  check_not_hybrid_result(
    last(c), a = 2:4,
    expected = 3L
  )
  check_not_hybrid_result(
    nth(c, 2), a = 2:4,
    expected = 2L
  )

  skip("Currently failing (segfault)")
  check_not_hybrid_result(
    first(a, order_by = b), a = 1:5, b = 5:1,
    expected = 5L
  )
})

test_that("lead() and lag() work", {
  check_hybrid_result(
    list(lead(a)), a = 1:5,
    expected = list(c(2:5, NA))
  )
  check_hybrid_result(
    list(lag(a)), a = 1:5,
    expected = list(c(NA, 1:4))
  )

  check_hybrid_result(
    list(lead(a)), a = as.numeric(1:5),
    expected = list(c(as.numeric(2:5), NA))
  )
  check_hybrid_result(
    list(lag(a)), a = as.numeric(1:5),
    expected = list(c(NA, as.numeric(1:4)))
  )

  check_hybrid_result(
    list(lead(a)), a = letters[1:5],
    expected = list(c(letters[2:5], NA))
  )
  check_hybrid_result(
    list(lag(a)), a = letters[1:5],
    expected = list(c(NA, letters[1:4]))
  )

  check_hybrid_result(
    list(lead(a)), a = c(TRUE, FALSE),
    expected = list(c(FALSE, NA))
  )
  check_hybrid_result(
    list(lag(a)), a = c(TRUE, FALSE),
    expected = list(c(NA, TRUE))
  )

  check_not_hybrid_result(
    list(lead(a, default = 2 + 4)), a = 1:5,
    expected = list(as.numeric(2:6))
  )
  check_not_hybrid_result(
    list(lag(a, default = 3L - 3L)), a = as.numeric(1:5),
    expected = list(as.numeric(0:4))
  )

  check_not_hybrid_result(
    list(lead(a, order_by = b)), a = 1:5, b = 5:1,
    expected = list(c(NA, 1:4))
  )
  check_not_hybrid_result(
    list(lag(a, order_by = b)), a = 1:5, b = 5:1,
    expected = list(c(2:5, NA))
  )

  skip("Currently failing (complex)")
  check_hybrid_result(
    list(lead(a)), a = 1:5 * 1i,
    expected = list(c(2:5, NA) * 1i)
  )
  check_hybrid_result(
    list(lag(a)), a = 1:5 * 1i,
    expected = list(c(NA, 1:4) * 1i)
  )

  skip("Currently failing")
  check_hybrid_result(
    list(lead(a, 1L + 2L)), a = 1:5,
    expected = list(c(4:5, NA, NA, NA))
  )
  check_hybrid_result(
    list(lag(a, 4L - 2L)), a = as.numeric(1:5),
    expected = list(c(NA, NA, as.numeric(1:3)))
  )

  check_hybrid_result(
    list(lead(a, 1 + 2)), a = 1:5,
    expected = list(c(4:5, NA, NA, NA))
  )
  check_hybrid_result(
    list(lag(a, 4 - 2)), a = as.numeric(1:5),
    expected = list(c(NA, NA, as.numeric(1:3)))
  )

  check_hybrid_result(
    list(lead(a, default = 2L + 4L)), a = 1:5,
    expected = list(2:6)
  )
  check_hybrid_result(
    list(lag(a, default = 3L - 3L)), a = 1:5,
    expected = list(0:4)
  )

  check_hybrid_result(
    list(lead(a, def = 2L + 4L)), a = 1:5,
    expected = list(2:6)
  )
  check_hybrid_result(
    list(lag(a, def = 3L - 3L)), a = 1:5,
    expected = list(0:4)
  )

  check_hybrid_result(
    list(lead(a, 2, 2L + 4L)), a = 1:5,
    expected = list(c(3:6, 6L))
  )
  check_hybrid_result(
    list(lag(a, 3, 3L - 3L)), a = 1:5,
    expected = list(c(0L, 0L, 0:2))
  )
})

test_that("mean(), var(), sd() and sum() work", {
  check_hybrid_result(
    mean(a), a = 1:5,
    expected = 3
  )
  check_hybrid_result(
    var(a), a = 1:3,
    expected = 1
  )
  check_hybrid_result(
    sd(a), a = 1:3,
    expected = 1
  )
  check_hybrid_result(
    sum(a), a = 1:5,
    expected = 15L
  )
  check_hybrid_result(
    sum(a), a = as.numeric(1:5),
    expected = 15
  )

  check_hybrid_result(
    mean(a), a = c(1:5, NA),
    expected = NA_real_
  )
  check_hybrid_result(
    var(a), a = c(1:3, NA),
    expected = NA_real_
  )
  check_hybrid_result(
    sum(a), a = c(1:5, NA),
    expected = NA_integer_
  )
  check_hybrid_result(
    sum(a), a = c(as.numeric(1:5), NA),
    expected = NA_real_
  )

  check_not_hybrid_result(
    sd(a, TRUE), a = c(1:3, NA),
    expected = 1
  )

  check_not_hybrid_result(
    sd(a, na.rm = b[[1]]), a = c(1:3, NA), b = TRUE,
    expected = 1
  )

  skip("Currently failing, sqrt(NA) in sd()")
  check_hybrid_result(
    sd(a), a = c(1:3, NA),
    expected = NA_real_
  )

  skip("Currently failing")
  check_hybrid_result(
    mean(a, na.rm = (1 == 0)), a = c(1:5, NA),
    expected = NA_real_
  )
  check_hybrid_result(
    var(a, na.rm = (1 == 0)), a = c(1:3, NA),
    expected = NA_real_
  )
  check_hybrid_result(
    sd(a, na.rm = (1 == 0)), a = c(1:3, NA),
    expected = NA_real_
  )
  check_hybrid_result(
    sum(a, na.rm = (1 == 0)), a = c(1:5, NA),
    expected = NA_integer_
  )
  check_hybrid_result(
    sum(a, na.rm = (1 == 0)), a = c(as.numeric(1:5), NA),
    expected = NA_real_
  )

  check_hybrid_result(
    mean(a, na.rm = (1 == 1)), a = c(1:5, NA),
    expected = 3
  )
  check_hybrid_result(
    var(a, na.rm = (1 == 1)), a = c(1:3, NA),
    expected = 1
  )
  check_hybrid_result(
    sd(a, na.rm = (1 == 1)), a = c(1:3, NA),
    expected = 1
  )
  check_hybrid_result(
    sum(a, na.rm = (1 == 1)), a = c(1:5, NA),
    expected = 15L
  )
  check_hybrid_result(
    sum(a, na.rm = (1 == 1)), a = c(as.numeric(1:5), NA),
    expected = 15
  )

  check_hybrid_result(
    mean(na.rm = (1 == 1), a), a = c(1:5, NA),
    expected = 3
  )
  check_hybrid_result(
    var(na.rm = (1 == 1), a), a = c(1:3, NA),
    expected = 1
  )
  check_hybrid_result(
    sd(na.rm = (1 == 1), a), a = c(1:3, NA),
    expected = 1
  )
  check_hybrid_result(
    sum(na.rm = (1 == 1), a), a = c(1:5, NA),
    expected = 15L
  )
  check_hybrid_result(
    sum(na.rm = (1 == 1), a), a = c(as.numeric(1:5), NA),
    expected = 15
  )
})

test_that("row_number(), ntile(), min_rank(), percent_rank(), dense_rank(), and cume_dist() work", {
  check_hybrid_result(
    list(row_number()), a = 1:5,
    expected = list(1:5),
    test_eval = FALSE
  )
  check_hybrid_result(
    list(row_number(a)), a = 5:1,
    expected = list(5:1)
  )
  check_hybrid_result(
    list(min_rank(a)), a = c(1, 3, 2, 3, 1),
    expected = list(c(1L, 4L, 3L, 4L, 1L))
  )
  check_hybrid_result(
    list(percent_rank(a)), a = c(1, 3, 2, 3, 1),
    expected = list((c(1L, 4L, 3L, 4L, 1L) - 1) / 4)
  )
  check_hybrid_result(
    list(cume_dist(a)), a = c(1, 3, 2, 3),
    expected = list(c(0.25, 1.0, 0.5, 1.0))
  )
  check_hybrid_result(
    list(dense_rank(a)), a = c(1, 3, 2, 3, 1),
    expected = list(c(1L, 3L, 2L, 3L, 1L))
  )

  expect_not_hybrid_error(
    row_number(a, 1), a = 5:1,
    error = "unused argument"
  )
  expect_not_hybrid_error(
    min_rank(a, 1), a = 5:1,
    error = "unused argument"
  )
  expect_not_hybrid_error(
    percent_rank(a, 1), a = 5:1,
    error = "unused argument"
  )
  expect_not_hybrid_error(
    cume_dist(a, 1), a = 5:1,
    error = "unused argument"
  )
  expect_not_hybrid_error(
    dense_rank(a, 1), a = 5:1,
    error = "unused argument"
  )
  expect_not_hybrid_error(
    ntile(a, 2, 1), a = 5:1,
    error = "unused argument"
  )

  check_not_hybrid_result(
    row_number("a"), a = 5:1,
    expected = 1L
  )
  check_not_hybrid_result(
    min_rank("a"), a = 5:1,
    expected = 1L
  )
  check_not_hybrid_result(
    percent_rank("a"), a = 5:1,
    expected = is.nan
  )
  check_not_hybrid_result(
    cume_dist("a"), a = 5:1,
    expected = 1
  )
  check_not_hybrid_result(
    dense_rank("a"), a = 5:1,
    expected = 1L
  )
  check_not_hybrid_result(
    ntile("a", 2), a = 5:1,
    expected = 1L
  )

  skip("Currently failing (constfold)")
  check_hybrid_result(
    list(ntile(a, 1 + 2)), a = c(1, 3, 2, 3, 1),
    expected = list(c(1L, 2L, 2L, 3L, 1L))
  )
  check_hybrid_result(
    list(ntile(a, 1L + 2L)), a = c(1, 3, 2, 3, 1),
    expected = list(c(1L, 2L, 2L, 3L, 1L))
  )
  check_hybrid_result(
    list(ntile(n = 1 + 2, a)), a = c(1, 3, 2, 3, 1),
    expected = list(c(1L, 2L, 2L, 3L, 1L))
  )
})

test_that("hybrid handlers don't nest", {
  check_not_hybrid_result(
    mean(lag(a)), a = 1:5,
    expected = NA_real_
  )
  check_not_hybrid_result(
    mean(row_number()), a = 1:5,
    expected = 3,
    test_eval = FALSE
  )
  check_not_hybrid_result(
    list(lag(cume_dist(a))), a = 1:4,
    expected = list(c(NA, 0.25, 0.5, 0.75))
  )
})
