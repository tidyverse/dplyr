test_that("generate informative errors", {
  expect_snapshot(error = TRUE, {
    id()

    failwith()

    select_vars()
    rename_vars()
    select_var()
    current_vars()

    bench_tbls()
    compare_tbls()
    compare_tbls2()
    eval_tbls()
    eval_tbls2()

    location()
    changes()
  })
})
