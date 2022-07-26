test_that("database srcs are defunct", {
  expect_snapshot(error = TRUE, {
    src_postgres()
    src_mysql()
    src_sqlite()
  })
})
