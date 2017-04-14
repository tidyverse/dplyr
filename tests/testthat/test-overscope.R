context("overscope")

test_that(".data has strict matching semantics (#2591)", {
  expect_error(
    data_frame(a = 1) %>% mutate(c = .data$b),
    "Column `b`: not found in data"
  )

  expect_error(
    data_frame(a = 1:3) %>% group_by(a) %>% mutate(c = .data$b),
    "Column `b`: not found in data"
  )
})
