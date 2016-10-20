context("hybrid-traverse")

test_df <- data_frame(
  id = c(1L, 2L, 2L),
  a = 1:3,
  b = as.numeric(1:3),
  c = letters[1:3],
  d = c(TRUE, FALSE, NA),
  e = list(list(a = 1, x = 2), list(a = 2, x = 3), list(a = 3, x = 4))
)

test_hybrid <- function(rowwise) {

test_that("$ is parsed correctly (#1400)", {
  expect_equal(
    test_df %>%
      rowwise %>%
      mutate(f = e$x) %>%
      select(-e),
    test_df %>%
      mutate(f = as.numeric(2:4)) %>%
      rowwise %>%
      select(-e))
})

test_that("$ is parsed correctly if column by the same name exists (#1400)", {
  expect_equal(
    test_df %>%
      rowwise %>%
      mutate(f = e$a) %>%
      select(-e),
    test_df %>%
      mutate(f = as.numeric(1:3)) %>%
      rowwise %>%
      select(-e))
})

test_that("case_when() works for LHS (#1719)", {
  expect_equal(
    test_df %>%
      rowwise %>%
      mutate(f = case_when(a == 1 ~ 1, a == 2 ~ 2, TRUE ~ 3)) %>%
      select(-e),
    test_df %>%
      mutate(f = b) %>%
      rowwise %>%
      select(-e))
})

test_that("case_when() works for RHS (#1719)", {
  expect_equal(
    test_df %>%
      rowwise %>%
      mutate(f = case_when(a == 1 ~ as.numeric(a), a == 2 ~ b, TRUE ~ 3)) %>%
      select(-e),
    test_df %>%
      mutate(f = b) %>%
      rowwise %>%
      select(-e))
})

test_that("assignments work (#1452)", {
  expect_equal(
    test_df %>%
      rowwise %>%
      mutate(f = { a <- 5; a }) %>%
      select(-e),
    test_df %>%
      mutate(f = 5) %>%
      rowwise %>%
      select(-e))
})

test_that("[ works (#912)", {
  expect_equal(
    test_df %>%
      rowwise %>%
      mutate(f = test_df["a"]) %>%
      select(-e),
    test_df %>%
      mutate(f = a) %>%
      rowwise %>%
      select(-e))
})

}

test_hybrid(identity)
test_hybrid(rowwise)
test_hybrid(. %>% group_by_(~id))
