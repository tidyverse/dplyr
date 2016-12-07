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

  c <- 1:3
  expect_not_hybrid(n_distinct(c), a = 1:5,
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
