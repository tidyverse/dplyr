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
  grouping <- rowwise

  expect_equal(
    test_df %>%
      grouping %>%
      mutate(f = e$x) %>%
      select(-e),
    test_df %>%
      mutate(f = as.numeric(2:4)) %>%
      grouping %>%
      select(-e))
})

test_that("$ is parsed correctly if column by the same name exists (#1400)", {
  grouping <- rowwise

  expect_equal(
    test_df %>%
      grouping %>%
      mutate(f = e$a) %>%
      select(-e),
    test_df %>%
      mutate(f = as.numeric(1:3)) %>%
      grouping %>%
      select(-e))
})

test_hybrid <- function(grouping) {

test_that("case_when() works for LHS (#1719)", {
  expect_equal(
    test_df %>%
      grouping %>%
      mutate(f = case_when(a == 1 ~ 1, a == 2 ~ 2, TRUE ~ 3)) %>%
      select(-e),
    test_df %>%
      mutate(f = b) %>%
      grouping %>%
      select(-e))
})

test_that("case_when() works for RHS (#1719)", {
  expect_equal(
    test_df %>%
      grouping %>%
      mutate(f = case_when(a == 1 ~ as.numeric(a), a == 2 ~ b, TRUE ~ 3)) %>%
      select(-e),
    test_df %>%
      mutate(f = b) %>%
      grouping %>%
      select(-e))
})

test_that("assignments work (#1452)", {
  expect_equal(
    test_df %>%
      grouping %>%
      mutate(f = { xx <- 5; xx }) %>%
      select(-e),
    test_df %>%
      mutate(f = 5) %>%
      grouping %>%
      select(-e))
})

test_that("assignments don't carry over (#1452)", {
  expect_error(
    test_df %>%
      grouping %>%
      mutate(f = { xx <- 5; xx }, g = xx),
    "xx")
})

test_that("assignments don't leak (#1452)", {
  expect_false(exists("xx"))
  test <-
    test_df %>%
    grouping %>%
    mutate(f = { xx <- 5; xx })
  expect_false(exists("xx"))
})

test_that("assignments still throws error if variable is affected (#315)", {
  expect_error(
    test_df %>%
      grouping %>%
      mutate(f = { a <- 5; a }),
    "read-only")
})

test_that("[ works (#912)", {
  grouped_df <- test_df %>%
    grouping

  expect_equal(
    grouped_df %>%
      mutate(f = mean(grouped_df["a"][[1]])) %>%
      select(-e),
    test_df %>%
      mutate(f = mean(a)) %>%
      grouping %>%
      select(-e))
})

test_that("[[ works (#912)", {
  if (!identical(grouping, identity)) {
    skip("Need to override column accessor functions for this to work.")
  }

  expect_equal(
    test_df %>%
      grouping %>%
      mutate(f = mean(test_df[["a"]])) %>%
      select(-e),
    test_df %>%
      mutate(f = mean(a)) %>%
      grouping %>%
      select(-e))
})

test_that("interpolation works (#1012)", {
  skip("lazyeval#78")

  df_mean <- function(df, variable) {
    lazyeval::f_eval(~ mean(lazyeval::uq(variable)), data = df)
  }

  expect_equal(
    test_df %>%
      grouping %>%
      mutate(f = df_mean(test_df, ~b)) %>%
      select(-e),
    test_df %>%
      grouping %>%
      mutate(f = mean(b)) %>%
      select(-e))
})

}

test_hybrid(identity)
test_hybrid(rowwise)
test_hybrid(. %>% group_by_(~id))
