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
