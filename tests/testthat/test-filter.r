context("Filter")

df <- expand.grid(a = 1:10, b = letters[1:10],
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE)

srcs <- temp_srcs(c("df", "dt", "cpp", "sqlite", "postgres"))
tbls <- temp_load(srcs, df)

test_that("filter results independent of data tbl (simple)", {
  expected <- df[df$a > 6, , drop = FALSE]
  compare_tbls(tbls, function(x) x %.% filter(a > 6), ref = expected)
})

test_that("filter captures local variables", {
  sel <- c("d", "g", "a")
  expected <- df[df$b %in% sel, , drop = FALSE]

  compare_tbls(tbls, function(x) x %.% filter(b %in% sel), ref = expected)
})

test_that("two filters equivalent to one", {
  expected <- filter(df, a > 4 & b == "a")
  
  compare_tbls(tbls, function(x) x %.% filter(a > 4) %.% filter(b == "a"), 
    ref = expected)
})

test_that("filter fails if inputs incorrect length (#156)", {
  expect_error( filter(tbl_df(mtcars), c(F, T)) )
  expect_error( filter(group_by(mtcars, am), c(F, T)) )
})
