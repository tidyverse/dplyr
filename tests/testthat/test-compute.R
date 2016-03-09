context("Compute")

test_that("compute doesn't change representation", {
  tbls <- test_frame(x = 5:1, y = 1:5, z = "a")

  compare_tbls(tbls, . %>% compute, convert = TRUE)
  compare_tbls(tbls, . %>% mutate(a = x) %>% compute, convert = TRUE)
})

test_that("compute can create indexes", {
  tbls <- test_frame(x = 5:1, y = 1:5, z = "a")

  compare_tbls(tbls, . %>% compute(indexes = c("x", "y")),
               compare = equal_data_frame, convert = TRUE)
  compare_tbls(tbls, . %>% compute(indexes = list("x", "y", c("x", "y"))),
               compare = equal_data_frame, convert = TRUE)
  compare_tbls(tbls, . %>% compute(indexes = "x", unique_indexes = "y"),
               compare = equal_data_frame, convert = TRUE)
  compare_tbls(tbls, . %>% compute(unique_indexes = c("x", "y")),
               compare = equal_data_frame, convert = TRUE)
  compare_tbls(tbls, . %>% compute(unique_indexes = list(c("x", "z"), c("y", "z"))),
               compare = equal_data_frame, convert = TRUE)
  # FIXME: Reenable Postgres when it starts throwing errors on execution
  # failures
  compare_tbls(
    tbls[!(names(tbls) %in% c("df", "postgres"))],
    function(tbl) {
      expect_error(compute(tbl, unique_indexes = "z"), ".")
      expect_error(compute(tbl, indexes = "x", unique_indexes = "z"))
      data_frame()
    },
    ref = data_frame()
  )
})
