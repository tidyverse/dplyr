test_that("standard summary returns correct results", {
  # Create a test data frame
  df <- data.frame(
    x = c(1, 2, 3, 4, 5),
    y = c(6, 7, 8, 9, 10)
  )
  
  # Call the standard summary function
  result <- standard_summary(df)
  
  # Check if the result is a data frame
  expect_is(result, "data.frame")
  
  # Check if the result has the correct number of rows and columns
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 2)
  
  # Check if the result contains the correct summary statistics
  expect_equal(result[1, "x"], 3)
  expect_equal(result[2, "y"], 8)
})