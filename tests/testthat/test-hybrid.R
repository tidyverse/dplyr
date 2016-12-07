context("hybrid")

test_df <- data_frame(
  id = c(1L, 2L, 2L),
  a = 1:3,
  b = as.numeric(1:3),
  c = letters[1:3],
  d = c(TRUE, FALSE, NA)
)

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
  # Hybrid evaluation currently requires constants (#2143)
  test_df_mutated <-
    mutate(test_df,
           a1 = a %in% 1L,
           b1 = b %in% 1,
           c1 = c %in% "a",
           d1 = d %in% TRUE
    )

  expected <- c(TRUE, FALSE, FALSE)
  expect_equal(test_df_mutated$a1, expected)
  expect_equal(test_df_mutated$b1, expected)
  expect_equal(test_df_mutated$c1, expected)
  expect_equal(test_df_mutated$d1, expected)
})
