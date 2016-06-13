context("Equivalence (manip)")


test_that("mutate happens before summarise", {
  test_f <- function(tbl) {
    res <- tbl %>%
      mutate(x, z = x + y) %>%
      summarise(sum_z = sum(z)) %>%
      collect()
    expect_equal(res$sum_z, 30)
  }

  test_frame(x = 5:1, y = 1:5) %>% lapply(test_f)
})

test_that("select operates on mutated vars", {
  test_f <- function(tbl) {
    res <- tbl %>%
      mutate(x, z = x + y) %>%
      select(z) %>%
      collect()
    expect_equal(res$z, rep(4, 3))
  }

  test_frame(x = 1:3, y = 3:1) %>% lapply(test_f)
})
