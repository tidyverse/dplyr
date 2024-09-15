test_that("standard summary returns correct results", {
  # Create a test data frame
  df <- data.frame(
   groups = c("a","a","a","b","c"),
   var1 = c(1, 2, 3, 4, 5),
   var2 = c(6, 7, NA, 9, 10)
  )

  # Calculate standard summary statistics for var1 and var2
  result <- starwars %>%
    filter(!is.na(sex)) %>%
    group_by(sex) %>%
    standard_summary(vars=c("height", "mass","birth_year"))

  # Check if the result is a data frame
  expect_true(is.data.frame(result))

# Check if the result has the correct number of rows and columns
  expect_equal(nrow(result), 12)
  expect_equal(df_n_col(result), 13)

  # Check if the result contains the correct summary statistics
  expect_equal(result[1,"min"], 150)
  expect_equal(result[2, "med"] , 55)
  expect_equal(result[7,"nmiss"],3)
})
