context("hybrid")

test_df <- data_frame(
  id = c(1L, 2L, 2L),
  a = 1:3,
  b = as.numeric(1:3),
  c = letters[1:3],
  d = c(TRUE, FALSE, NA)
)

test_that("%in% works", {
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
