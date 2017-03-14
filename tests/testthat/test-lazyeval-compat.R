context("lazyeval compatibility")

test_that("can select negatively (#2519)", {
  expect_identical(select_(mtcars, ~-cyl), mtcars[-2])
})

test_that("select yields proper names", {
  expect_identical(names(select_(mtcars, ~cyl:hp)), c("cyl", "disp", "hp"))
})

test_that("lazydots are named and arrange() doesn't fail (it assumes empty names)", {
  dots <- compat_lazy_dots(list(), env(), "cyl")
  expect_identical(names(dots), "")
  expect_identical(arrange_(mtcars, "cyl"), arrange(mtcars, cyl))
})

test_that("mutate_each_() and summarise_each_() handle lazydots", {
  expect_warning(cyl_chr <- mutate_each_(mtcars, funs(as.character), "cyl")$cyl, "deprecated")
  expect_identical(cyl_chr, as.character(mtcars$cyl))

  expect_warning(cyl_mean <- summarise_each_(mtcars, funs(mean), "cyl")$cyl, "deprecated")
  expect_equal(cyl_mean, mean(mtcars$cyl))
})
