context("Put")

test_that("put assigns to current environment",{
  put(mtcars, mtcars1)
  expect_true(exists("mtcars1"))
  mtcars %>% put(mtcars2)
  expect_true(exists("mtcars2"))
})

f <- function(dat, where = NULL) {
  put(dat, dat1)
  dat %>% put(dat2)
  put(dat, dat3, where = where)
  dat %>% put(dat4, where = where)
  c(exists("dat1"),
    exists("dat2"),
    exists("dat3", where = where),
    exists("dat4", where = where))
}

test_that("put assigns to local environment within functions", {
  x <- f(mtcars, where = environment())
  expect_true(all(x))
  expect_false(any(c(exists("dat1"), exists("dat2"))))
  expect_true(all(c(exists("dat3"), exists("dat4"))))
})

test_that("nested puts work", {
  put(f(mtcars, where = environment()), put_tests)
  expect_true(all(put_tests))
  expect_false(any(c(exists("dat1"), exists("dat2"))))
  expect_true(all(c(exists("dat3"), exists("dat4"))))
  rm(list=c("dat3","dat4"))

  f(mtcars, where = environment()) %>% put(put_tests)
  expect_true(all(put_tests))
  expect_false(any(c(exists("dat1"), exists("dat2"))))
  expect_true(all(c(exists("dat3"), exists("dat4"))))
})

test_that("where option works",{
  put(mtcars, mtcars_global, where = globalenv())
  expect_false(exists("mtcars_global", inherits = FALSE))
  expect_true(exists("mtcars_global", where = globalenv()))
})
