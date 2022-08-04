test_that("generate informative errors", {
  expect_snapshot(error = TRUE, {
    id()

    failwith()

    funs()

    select_vars()
    rename_vars()
    select_var()
    current_vars()

    bench_tbl()
    compare_tbls()
    compare_tbls2()
    eval_tbls()
    eval_tbls2()
  })
})
