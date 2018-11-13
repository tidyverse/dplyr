context("group_nest")

test_that("group_nest() works", {
  grouped <- group_by(starwars, species, homeworld)
  gdata <- group_data(grouped)

  res <- group_nest(starwars, species, homeworld)
  expect_is(pull(res), "list")
  expect_equal(select(res, -last_col()), select(gdata, -last_col()))
})

test_that("group_nest() works on grouped data frames", {
  grouped <- group_by(starwars, species, homeworld)
  gdata <- group_data(grouped)

  res <- group_nest(grouped)
  expect_is(pull(res), "list")
  expect_equal(select(res, -last_col()), select(gdata, -last_col()))
})

test_that("group_nest_at() works", {
  gdata <- group_data(group_by(starwars, species, homeworld))

  res <- group_nest_at(starwars, vars(c("species", "homeworld")))
  expect_is(pull(res), "list")
  expect_equal(select(res, -last_col()), select(gdata, -last_col()))
})

test_that("group_nest_if() works", {
  res <- group_nest_if(iris, is.factor)
  gdata <- group_data(group_by(iris, Species))
  expect_is(pull(res), "list")
  expect_equal(select(res, -last_col()), select(gdata, -last_col()))
})

test_that("group_split.grouped_df() warns about ...", {
  expect_warning(group_nest(group_by(mtcars, cyl), cyl))
})
