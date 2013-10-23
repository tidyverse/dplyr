context("Select")

df <- as.data.frame(as.list(setNames(1:26, letters)))
srcs <- temp_srcs(c("df", "dt", "cpp", "sqlite", "postgres"))
tbls <- temp_load(srcs, df)


test_that("two selects equivalent to one", {
  compare_tbls(tbls, function(tbl) tbl %.% select(l:s) %.% select(n:o),
    ref = select(df, n:o))
})
