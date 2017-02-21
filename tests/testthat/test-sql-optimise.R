context("SQL: optimise")

test_that("optimisation is turned on by default", {
  lf <- lazy_frame(x = 1, y = 2) %>% arrange(x) %>% head(5)
  qry <- lf %>% sql_build()

  expect_equal(qry$from, ident("df"))
})


# Optimisations -----------------------------------------------------------

test_that("group by then limit is collapsed", {
  lf <- memdb_frame(x = 1:10, y = 2) %>%
    group_by(x) %>%
    summarise(y = sum(y)) %>%
    head(1)

  qry <- lf %>% sql_build()
  expect_equal(qry$limit, 1L)
  expect_equal(qry$group_by, sql('"x"'))

  # And check that it returns the correct value
  expect_equal(collect(lf), tibble(x = 1L, y = 2))
})

test_that("filter and rename are correctly composed", {
  lf <- memdb_frame(x = 1, y = 2) %>%
    filter(x == 1) %>%
    select(x = y)

  qry <- lf %>% sql_build()
  expect_equal(qry$select, ident(x = "y"))
  expect_equal(qry$where, sql('"x" = 1.0'))

  # It surprises me that this SQL works!
  expect_equal(collect(lf), tibble(x = 2))
})
