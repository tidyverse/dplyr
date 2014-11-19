context("slice")

test_that( "slice handles numeric input (#226)", {
  g <- mtcars %>% group_by(cyl)
  res <- g %>% slice(1)
  expect_equal(nrow(res), 3)
  expect_equal(res, g %>% filter(row_number()==1L))

  expect_equal(
    mtcars %>% slice(1),
    mtcars %>% filter(row_number() == 1L)
  )
})

test_that( "slice silently ignores out of range values (#226)", {
  expect_equal( slice(mtcars, c(2,100)), slice(mtcars, 2))

  g <- group_by(mtcars, cyl)
  expect_equal( slice(g, c(2,100)), slice(g, 2))

})

test_that( "slice works with 0 args", {
  expect_equal(slice(mtcars), mtcars)
})

test_that("slice works with negative indices", {
  res <- slice(mtcars, -(1:2))
  exp <- tail(mtcars, -2)
  expect_equal(names(res), names(exp))
  for( col in names(res)){
    expect_equal( res[[col]], exp[[col]] )
  }
})

test_that("slice forbids positive and negative together", {
  expect_error(mtcars %>% slice(c(-1,2)))
})

test_that("slice works with grouped data", {
  g <- group_by(mtcars, cyl)

  res <- slice(g, 1:2)
  exp <- filter(g, row_number() < 3)
  expect_equal(res,exp)

  res <- slice(g, -(1:2))
  exp <- filter(g, row_number() >= 3)
  expect_equal(res,exp)

})

test_that("slice gives correct rows (#649)", {
  a <- data_frame(value = paste0("row", 1:10))
  expect_equal( slice(a, 1:3)$value, paste0("row", 1:3) )
  expect_equal( slice(a, c(4, 6, 9))$value, paste0("row", c(4,6,9)))

  a <- data_frame(value = paste0("row", 1:10), group = rep(1:2, each = 5)) %>% group_by(group)

  expect_equal( slice(a, 1:3)$value, paste0("row", c(1:3,6:8) ) )
  expect_equal( slice(a, c(2,4))$value, paste0("row", c(2,4,7,9)))
})

# Data tables ------------------------------------------------------------------

test_that("slicing data.table yields same output as slicing data.frame ", {
  tbls <- list(mtcars, mtcars %>% tbl_dt())
  compare_tbls(tbls, function(x) x %>% group_by(cyl) %>% slice(c(1, 3)))
})

test_that("slicing data table preserves input class", {
  mtcars_dt <- mtcars %>% data.table::data.table()

  expect_is(mtcars_dt %>% slice(1), "data.table")
  expect_is(mtcars_dt %>% tbl_dt() %>% slice(1), "tbl_dt")
  expect_is(mtcars_dt %>% group_by(cyl) %>% slice(1), "grouped_dt")
})
