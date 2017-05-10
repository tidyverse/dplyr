context("Set ops")

test_that("set operation give useful error message. #903", {
  alfa <- data_frame(
    land = c("Sverige", "Norway", "Danmark", "Island", "GB"),
    data = rnorm(length(land))
  )

  beta <- data_frame(
    land = c("Norge", "Danmark", "Island", "Storbritannien"),
    data2 = rnorm(length(land))
  )
  expect_error(
    intersect(alfa, beta),
    "not compatible: \n- Cols in y but not x: `data2`. \n- Cols in x but not y: `data`. \n",
    fixed = TRUE
  )
  expect_error(
    union(alfa, beta),
    "not compatible: \n- Cols in y but not x: `data2`. \n- Cols in x but not y: `data`. \n",
    fixed = TRUE
  )
  expect_error(
    setdiff(alfa, beta),
    "not compatible: \n- Cols in y but not x: `data2`. \n- Cols in x but not y: `data`. \n",
    fixed = TRUE
  )
})

test_that("set operations use coercion rules (#799)", {
  df1 <- data_frame(x = 1:2, y = c(1, 1))
  df2 <- data_frame(x = 1:2, y = 1:2)

  expect_equal(nrow(union(df1, df2)), 3L)
  expect_equal(nrow(intersect(df1, df2)), 1L)
  expect_equal(nrow(setdiff(df1, df2)), 1L)

  df1 <- data_frame(x = factor(letters[1:10]))
  df2 <- data_frame(x = letters[6:15])
  expect_warning(res <- intersect(df1, df2))
  expect_equal(res, data_frame(x = letters[6:10]))
  expect_warning(res <- intersect(df2, df1))
  expect_equal(res, data_frame(x = letters[6:10]))

  expect_warning(res <- union(df1, df2))
  expect_equal(res, data_frame(x = letters[1:15]))
  expect_warning(res <- union(df2, df1))
  expect_equal(res, data_frame(x = letters[1:15]))

  expect_warning(res <- setdiff(df1, df2))
  expect_equal(res, data_frame(x = letters[1:5]))
  expect_warning(res <- setdiff(df2, df1))
  expect_equal(res, data_frame(x = letters[11:15]))
})

test_that("setdiff handles factors with NA (#1526)", {
  df1 <- data_frame(x = factor(c(NA, "a")))
  df2 <- data_frame(x = factor("a"))

  res <- setdiff(df1, df2)
  expect_is(res$x, "factor")
  expect_equal(levels(res$x), "a")
  expect_true(is.na(res$x[1]))
})

test_that("intersect does not unnecessarily coerce (#1722)", {
  df <- data_frame(a = 1L)
  res <- intersect(df, df)
  expect_is(res$a, "integer")
})

