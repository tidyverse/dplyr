context("overscope")

test_that(".data has strict matching semantics (#2591)", {
  # testthat says to use class =
  # but I guess older versions of R don't have the newest testthat
  # because that gives me an error
  suppressWarnings(expect_error(
    tibble(a = 1) %>% mutate(c = .data$b),
    "Column `b` not found in `.data`"
  ))

  suppressWarnings(expect_error(
    tibble(a = 1:3) %>% group_by(a) %>% mutate(c = .data$b),
    "Column `b` not found in `.data`"
  ))
})
