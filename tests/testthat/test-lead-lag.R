context("Lead and lag")

test_that("lead and lag preserve factors", {
  x <- factor(c("a", "b", "c"))

  expect_equal(levels(lead(x)), c("a", "b", "c"))
  expect_equal(levels(lag(x)), c("a", "b", "c"))
})

test_that("lead and lag preserves dates and times", {
  x <- as.Date("2013-01-01") + 1:3
  y <- as.POSIXct(x)

  expect_is(lead(x), "Date")
  expect_is(lag(x),  "Date")

  expect_is(lead(y), "POSIXct")
  expect_is(lag(y),  "POSIXct")
})

test_that("#925 is fixed", {
  data <- data_frame( 
    name = c("Rob", "Pete", "Rob", "John", "Rob", "Pete", "John", "Pete", "John", "Pete", "Rob", "Rob"), 
    time = c(3, 2, 5, 3, 2, 3, 2, 4, 1, 1, 4, 1)
  )
  res <- data %>% group_by(name) %>% mutate( lag_time = lag(time) )
  expect_equal( res$lag_time[ res$name == "Rob" ] , c(NA, head( data$time[data$name == "Rob"] , -1 ) ) )
  expect_equal( res$lag_time[ res$name == "Pete" ], c(NA, head( data$time[data$name == "Pete"], -1 ) ) )
  expect_equal( res$lag_time[ res$name == "John" ], c(NA, head( data$time[data$name == "John"], -1 ) ) )
})

