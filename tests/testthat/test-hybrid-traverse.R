context("hybrid-traverse")

test_df <- data_frame(
  id = c(1L, 2L, 2L),
  a = 1:3,
  b = as.numeric(1:3),
  c = letters[1:3],
  d = c(TRUE, FALSE, NA),
  e = list(list(a = 1, x = 2), list(a = 2, x = 3), list(a = 3, x = 4))
)

test_that("$ is parsed correctly (#1400)", {
  skip("#1400")
  expect_equal(
    test_df %>%
      rowwise %>%
      mutate(f = e$x),
    test_df %>%
      mutate(f = as.numeric(2:4)) %>%
      group_by(id))
})

test_that("$ is parsed correctly if column by the same name exists (#1400)", {
  skip("#1400")
  expect_equal(
    test_df %>%
      rowwise %>%
      mutate(f = e$a),
    test_df %>%
      mutate(f = as.numeric(1:3)) %>%
      group_by(id))
})
