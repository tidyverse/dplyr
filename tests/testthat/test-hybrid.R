context("hybrid")

test_that("n() and n_distinct() work", {
  expect_hybrid(n(), a = 1:5, expected = 5L, test_eval = FALSE)

  expect_hybrid(n_distinct(a), a = 1:5,
                expected = 5L)
  expect_hybrid(n_distinct(a), a = rep(1L, 3),
                expected = 1L)
  expect_hybrid(n_distinct(a, b), a = rep(1L, 3), b = 1:3,
                expected = 3L)
  expect_hybrid(n_distinct(a, b), a = rep(1L, 3), b = c(1, 1, 2),
                expected = 2L)
  expect_hybrid(n_distinct(a, b), a = rep(1L, 3), b = c(1, 1, NA),
                expected = 2L)
  expect_hybrid(n_distinct(a, b, na.rm = TRUE), a = rep(1L, 3), b = c(1, 1, NA),
                expected = 1L)
  expect_hybrid(n_distinct(a = a, b = b, na.rm = TRUE), a = rep(1L, 3), b = c(1, 1, NA),
                expected = 1L)

  c <- 1:3
  expect_not_hybrid(n_distinct(c), a = 1:5,
                    expected = 3L, test_eval = FALSE)
  expect_not_hybrid(n_distinct(a, c), a = 1:3,
                    expected = 3L, test_eval = FALSE)
})

test_that("%in% works (#192)", {
  expect_hybrid(list(a %in% 1:3), a = 2:4,
                expected = list(c(TRUE, TRUE, FALSE)))
  expect_hybrid(list(a %in% as.numeric(1:3)), a = as.numeric(2:4),
                expected = list(c(TRUE, TRUE, FALSE)))
  expect_hybrid(list(a %in% letters[1:3]), a = letters[2:4],
                expected = list(c(TRUE, TRUE, FALSE)))
  expect_hybrid(list(a %in% c(TRUE, FALSE)), a = c(TRUE, FALSE, NA),
                expected = list(c(TRUE, TRUE, FALSE)))

  expect_not_hybrid(list(a %in% 1:3), a = as.numeric(2:4),
                    expected = list(c(TRUE, TRUE, FALSE)))
  expect_not_hybrid(list(a %in% as.numeric(1:3)), a = 2:4,
                expected = list(c(TRUE, TRUE, FALSE)))

  skip("Currently failing")
  expect_hybrid(list(a %in% NA_integer_), a = c(2:4, NA),
                expected = list(c(FALSE, FALSE, FALSE, TRUE)))
  expect_hybrid(list(a %in% NA_real_), a = as.numeric(c(2:4, NA)),
                expected = list(c(FALSE, FALSE, FALSE, TRUE)))
  expect_hybrid(list(a %in% NA_character_), a = c(letters[2:4], NA),
                expected = list(c(FALSE, FALSE, FALSE, TRUE)))
  expect_hybrid(list(a %in% NA), a = c(TRUE, FALSE, NA),
                expected = list(c(FALSE, FALSE, TRUE)))
})

test_that("min() and max() work", {
  expect_hybrid(min(a), a = 1:5,
                expected = 1L)
  expect_hybrid(max(a), a = 1:5,
                expected = 5L)
  expect_hybrid(min(a), a = as.numeric(1:5),
                expected = 1)
  expect_hybrid(max(a), a = as.numeric(1:5),
                expected = 5)
  expect_hybrid(min(a), a = c(1:5, NA),
                expected = NA_integer_)
  expect_hybrid(max(a), a = c(1:5, NA),
                expected = NA_integer_)
  expect_hybrid(min(a, na.rm = (1 == 0)), a = c(1:5, NA),
                expected = NA_integer_)
  expect_hybrid(max(a, na.rm = (1 == 0)), a = c(1:5, NA),
                expected = NA_integer_)
  expect_hybrid(min(a, na.rm = (1 == 1)), a = c(1:5, NA),
                expected = 1L)
  expect_hybrid(max(a, na.rm = (1 == 1)), a = c(1:5, NA),
                expected = 5L)

  expect_not_hybrid(min(a), a = letters,
                    expected = "a")
  expect_not_hybrid(max(a), a = letters,
                    expected = "z")
  expect_not_hybrid(min(a), a = c(letters, NA),
                    expected = NA_character_)
  expect_not_hybrid(max(a), a = c(letters, NA),
                    expected = NA_character_)
  expect_not_hybrid(min(a, na.rm = TRUE), a = c(letters, NA),
                    expected = "a")
  expect_not_hybrid(max(a, na.rm = TRUE), a = c(letters, NA),
                    expected = "z")
})

test_that("first(), last(), and nth() work", {
  expect_hybrid(first(a), a = 1:5,
                expected = 1L)
  expect_hybrid(last(a), a = as.numeric(1:5),
                expected = 5)
  expect_hybrid(nth(a, 1 + 2), a = letters[1:5],
                expected = "c")
  expect_hybrid(nth(a, 6, default = 3L), a = as.numeric(1:5),
                expected = 3)
  expect_hybrid(nth(a, 6, def = 3L), a = as.numeric(1:5),
                expected = 3)
  expect_hybrid(nth(a, 6.5), a = 1:5,
                expected = NA_integer_)
  expect_hybrid(nth(a, -4), a = 1:5,
                expected = 2L)

  expect_not_hybrid(nth(a, b[[2]]), a = letters[1:5], b = 5:1,
                    expected = "d")
  expect_not_hybrid(nth(a, 2), a = as.list(1:5),
                    expected = 2L)

  skip("Currently failing")
  expect_hybrid(nth(a, order_by = 5:1, 2), a = 1:5,
                expected = 4L)
  expect_hybrid(first(a, order_by = b), a = 1:5, b = 5:1,
                expected = 5L)
})

test_that("lead() and lag() work", {
  expect_hybrid(list(lead(a)), a = 1:5,
                expected = list(c(2:5, NA)))
  expect_hybrid(list(lag(a)), a = 1:5,
                expected = list(c(NA, 1:4)))

  expect_hybrid(list(lead(a)), a = as.numeric(1:5),
                expected = list(c(as.numeric(2:5), NA)))
  expect_hybrid(list(lag(a)), a = as.numeric(1:5),
                expected = list(c(NA, as.numeric(1:4))))

  expect_hybrid(list(lead(a)), a = 1:5 * 1i,
                expected = list(c(2:5, NA) * 1i))
  expect_hybrid(list(lag(a)), a = 1:5 * 1i,
                expected = list(c(NA, 1:4) * 1i))

  expect_hybrid(list(lead(a)), a = letters[1:5],
                expected = list(c(letters[2:5], NA)))
  expect_hybrid(list(lag(a)), a = letters[1:5],
                expected = list(c(NA, letters[1:4])))

  expect_hybrid(list(lead(a)), a = c(TRUE, FALSE),
                expected = list(c(FALSE, NA)))
  expect_hybrid(list(lag(a)), a = c(TRUE, FALSE),
                expected = list(c(NA, TRUE)))

  expect_hybrid(list(lead(a, 1L + 2L)), a = 1:5,
                expected = list(c(4:5, NA, NA, NA)))
  expect_hybrid(list(lag(a, 4L - 2L)), a = as.numeric(1:5),
                expected = list(c(NA, NA, as.numeric(1:3))))

  expect_hybrid(list(lead(a, 1 + 2)), a = 1:5,
                expected = list(c(4:5, NA, NA, NA)))
  expect_hybrid(list(lag(a, 4 - 2)), a = as.numeric(1:5),
                expected = list(c(NA, NA, as.numeric(1:3))))

  expect_hybrid(list(lead(a, default = 2L + 4L)), a = 1:5,
                expected = list(2:6))
  expect_hybrid(list(lag(a, default = 3L - 3L)), a = 1:5,
                expected = list(0:4))

  expect_hybrid(list(lead(a, def = 2L + 4L)), a = 1:5,
                expected = list(2:6))
  expect_hybrid(list(lag(a, def = 3L - 3L)), a = 1:5,
                expected = list(0:4))

  expect_hybrid(list(lead(a, 2, 2L + 4L)), a = 1:5,
                expected = list(c(3:6, 6L)))
  expect_hybrid(list(lag(a, 3, 3L - 3L)), a = 1:5,
                expected = list(c(0L, 0L, 0:2)))

  expect_not_hybrid(list(lead(a, default = 2 + 4)), a = 1:5,
                    expected = list(as.numeric(2:6)))
  expect_not_hybrid(list(lag(a, default = 3L - 3L)), a = as.numeric(1:5),
                    expected = list(as.numeric(0:4)))

  expect_not_hybrid(list(lead(a, order_by = b)), a = 1:5, b = 5:1,
                    expected = list(c(NA, 1:4)))
  expect_not_hybrid(list(lag(a, order_by = b)), a = 1:5, b = 5:1,
                    expected = list(c(2:5, NA)))
})

test_that("mean(), var(), sd() and sum() work", {
  expect_hybrid(mean(a), a = 1:5,
                expected = 3)
  expect_hybrid(var(a), a = 1:3,
                expected = 1)
  expect_hybrid(sd(a), a = 1:3,
                expected = 1)
  expect_hybrid(sum(a), a = 1:5,
                expected = 15L)
  expect_hybrid(sum(a), a = as.numeric(1:5),
                expected = 15)

  expect_hybrid(mean(a), a = c(1:5, NA),
                expected = NA_real_)
  expect_hybrid(var(a), a = c(1:3, NA),
                expected = NA_real_)
  expect_hybrid(sd(a), a = c(1:3, NA),
                expected = NA_real_)
  expect_hybrid(sum(a), a = c(1:5, NA),
                expected = NA_integer_)
  expect_hybrid(sum(a), a = c(as.numeric(1:5), NA),
                expected = NA_real_)

  expect_hybrid(mean(a, na.rm = (1 == 0)), a = c(1:5, NA),
                expected = NA_real_)
  expect_hybrid(var(a, na.rm = (1 == 0)), a = c(1:3, NA),
                expected = NA_real_)
  expect_hybrid(sd(a, na.rm = (1 == 0)), a = c(1:3, NA),
                expected = NA_real_)
  expect_hybrid(sum(a, na.rm = (1 == 0)), a = c(1:5, NA),
                expected = NA_integer_)
  expect_hybrid(sum(a, na.rm = (1 == 0)), a = c(as.numeric(1:5), NA),
                expected = NA_real_)

  expect_hybrid(mean(a, na.rm = (1 == 1)), a = c(1:5, NA),
                expected = 3)
  expect_hybrid(var(a, na.rm = (1 == 1)), a = c(1:3, NA),
                expected = 1)
  expect_hybrid(sd(a, na.rm = (1 == 1)), a = c(1:3, NA),
                expected = 1)
  expect_hybrid(sum(a, na.rm = (1 == 1)), a = c(1:5, NA),
                expected = 15L)
  expect_hybrid(sum(a, na.rm = (1 == 1)), a = c(as.numeric(1:5), NA),
                expected = 15)

  expect_hybrid(mean(na.rm = (1 == 1), a), a = c(1:5, NA),
                expected = 3)
  expect_hybrid(var(na.rm = (1 == 1), a), a = c(1:3, NA),
                expected = 1)
  expect_hybrid(sd(na.rm = (1 == 1), a), a = c(1:3, NA),
                expected = 1)
  expect_hybrid(sum(na.rm = (1 == 1), a), a = c(1:5, NA),
                expected = 15L)
  expect_hybrid(sum(na.rm = (1 == 1), a), a = c(as.numeric(1:5), NA),
                expected = 15)

  expect_not_hybrid(sd(a, TRUE), a = c(1:3, NA),
                    expected = 1)
})

test_that("row_number(), ntile(), min_rank(), percent_rank(), dense_rank(), and cume_dist() work", {
  expect_hybrid(list(row_number()), a = 1:5,
                expected = list(1:5),
                test_eval = FALSE)
  expect_hybrid(list(row_number(a)), a = 5:1,
                expected = list(5:1))
  expect_hybrid(list(min_rank(a)), a = c(1, 3, 2, 3, 1),
                expected = list(c(1L, 4L, 3L, 4L, 1L)))
  expect_hybrid(list(percent_rank(a)), a = c(1, 3, 2, 3, 1),
                expected = list((c(1L, 4L, 3L, 4L, 1L) - 1) / 4))
  expect_hybrid(list(cume_dist(a)), a = c(1, 3, 2, 3),
                expected = list(c(0.25, 1.0, 0.5, 1.0)))
  expect_hybrid(list(dense_rank(a)), a = c(1, 3, 2, 3, 1),
                expected = list(c(1L, 3L, 2L, 3L, 1L)))
  expect_hybrid(list(ntile(a, 1 + 2)), a = c(1, 3, 2, 3, 1),
                expected = list(c(1L, 2L, 2L, 3L, 1L)))
  expect_hybrid(list(ntile(a, 1L + 2L)), a = c(1, 3, 2, 3, 1),
                expected = list(c(1L, 2L, 2L, 3L, 1L)))
  expect_hybrid(list(ntile(n = 1 + 2, a)), a = c(1, 3, 2, 3, 1),
                expected = list(c(1L, 2L, 2L, 3L, 1L)))
})
