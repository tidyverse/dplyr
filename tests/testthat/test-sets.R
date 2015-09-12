context("Set ops")

tbls <- temp_load(c("df", "dt", "sqlite", "postgres"), mtcars)

test_that("results are the same across sources", {
  compare_tbls(tbls,
    function(x) setdiff(x, filter(x, cyl == 4)),
    filter(mtcars, cyl != 4)
  )
  compare_tbls(tbls,
    function(x) intersect(x, filter(x, cyl == 4)),
    filter(mtcars, cyl == 4)
  )
  compare_tbls(tbls,
    function(x) union(x, filter(x, cyl == 4)),
    mtcars)
  compare_tbls(tbls,
    function(x) union(filter(x, cyl == 6), filter(x, cyl == 4)),
    filter(mtcars, cyl %in% c(4, 6)))
})

test_that("set operation give useful error message. #903", {
  alfa <- data_frame(land=c("Sverige","Norway","Danmark","Island","GB"),
                     data=rnorm(length(land)))

  beta <- data_frame(land=c("Norge","Danmark","Island","Storbritannien"),
                     data2=rnorm(length(land)))
  expect_error( intersect(alfa, beta), "Cols in y but not x" )
  expect_error( union(alfa, beta), "Cols in y but not x" )
  expect_error( setdiff(alfa, beta), "Cols in y but not x" )
})

test_that("set operations use coercion rules (#799)", {
  df1 <- data_frame(x = 1:2, y = c(1, 1))
  df2 <- data_frame(x = 1:2, y = 1:2)

  expect_equal( nrow(union(df1, df2)), 3L )
  expect_equal( nrow(intersect(df1, df2)), 1L )
  expect_equal( nrow(setdiff(df1, df2)), 1L )

  df1 <- data.frame(x = letters[1:10])
  df2 <- data.frame(x = letters[6:15], stringsAsFactors = FALSE)
  expect_warning( { res <- intersect(df1, df2) })
  expect_equal( res, data_frame(x = letters[6:10]) )
  expect_warning( { res <- intersect(df2, df1) })
  expect_equal( res, data_frame(x = letters[6:10]) )


  expect_warning( { res <- union(df1, df2) })
  expect_equal( res, data_frame(x = letters[1:15]) )
  expect_warning( { res <- union(df2, df1) })
  expect_equal( res, data_frame(x = letters[1:15]) )

  expect_warning( { res <- setdiff(df1, df2) })
  expect_equal( res, data_frame(x = letters[1:5]) )
  expect_warning( { res <- setdiff(df2, df1) })
  expect_equal( res, data_frame(x = letters[11:15]) )

})
