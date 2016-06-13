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
