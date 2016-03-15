context("tbl_lazy")


# op_vars -----------------------------------------------------------------

test_that("select reduces variables", {
  out <- mtcars %>% tbl_lazy() %>% select(mpg:disp)
  expect_equal(op_vars(out), c("mpg", "cyl", "disp"))
})

test_that("rename preserves existing", {
  out <- data_frame(x = 1, y = 2) %>% tbl_lazy() %>% rename(z = y)
  expect_equal(op_vars(out), c("x", "z"))
})

test_that("mutate adds new", {
  out <- data_frame(x = 1) %>% tbl_lazy() %>% mutate(y = x + 1, z = y + 1)
  expect_equal(op_vars(out), c("x", "y", "z"))
})

test_that("summarise replaces existing", {
  out <- data.frame(x = 1, y = 2) %>% tbl_lazy() %>% summarise(z = 1)
  expect_equal(op_vars(out), "z")
})

test_that("grouped summary keeps groups", {
  out <- data.frame(g = 1, x = 1) %>%
    tbl_lazy() %>%
    group_by(g) %>%
    summarise(y = 1)
  expect_equal(op_vars(out), c("g", "y"))
})
