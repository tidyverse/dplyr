context("lazyeval compatibility")

test_that("can select negatively (#2519)", {
  scoped_lifecycle_silence()
  expect_identical(select_(mtcars, ~ -cyl), mtcars[-2])
})

test_that("select yields proper names", {
  scoped_lifecycle_silence()
  expect_identical(names(select_(mtcars, ~ cyl:hp)), c("cyl", "disp", "hp"))
})

test_that("lazydots are named and arrange() doesn't fail (it assumes empty names)", {
  scoped_lifecycle_silence()
  dots <- compat_lazy_dots(list(), env(), "cyl")
  expect_identical(names(dots), "")
  expect_identical(arrange_(mtcars, "cyl"), arrange(mtcars, cyl))
})

test_that("mutate_each_() and summarise_each_() handle lazydots", {
  scoped_lifecycle_silence()
  cyl_chr <- mutate_each_(mtcars, list(as.character), "cyl")$cyl
  expect_identical(cyl_chr, as.character(mtcars$cyl))

  cyl_mean <- summarise_each_(mtcars, list(mean), "cyl")$cyl
  expect_equal(cyl_mean, mean(mtcars$cyl))
})

test_that("select_vars_() handles lazydots", {
  scoped_lifecycle_silence()
  expect_identical(select_vars_(letters, c("a", "b")), set_names(c("a", "b")))
})
