test_that("generate informative errors", {
  expect_snapshot(error = TRUE, {
    combine()

    src_mysql()
    src_postgres()
    src_sqlite()

    src_local()
    src_df()

    tbl_df()
    as.tbl()
    add_rownames()
  })
})
