context("mutate sql")

test_that("can refer to fresly created values", {
  out1 <- memdb_frame(x = 1) %>%
    mutate(y = x + 1, z = y + 1) %>%
    collect()
  expect_equal(out1, tibble(x = 1, y = 2, z = 3))

  out2 <- memdb_frame(x = 1) %>%
    mutate(x = x + 1, x = x + 1) %>%
    collect()
  expect_equal(out2, tibble(x = 3))
})

test_that("queries are not nested unnecessarily", {
  # Should only be one query deep
  sql <- memdb_frame(x = 1) %>%
    mutate(y = x + 1, a = y + 1, b = y + 1) %>%
    sql_build()

  expect_s3_class(sql$from, "select_query")
  expect_s3_class(sql$from$from, "ident")
})

