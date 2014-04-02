context("top_n")

test_data <- function() {
  data.frame(
    name = rep(
      unlist(
        strsplit("this,is,to,make,sure,that,top_n,works,the,rightway", ",")), 10),
    values = 1:100)
}

test_that("top_n returns n rows", {
  test_df <- test_data()
  top_four <- test_df %.%
    group_by(name) %.%
    summarise(total = sum(values)) %.%
    top_n(4)
  expect_equal(dim(top_four), c(4, 2))
})
