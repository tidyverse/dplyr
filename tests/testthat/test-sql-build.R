context("SQL: build")

test_that("base source of lazy frame is always 'df'", {
  out <- lazy_frame(x = 1, y = 5) %>% sql_build()
  expect_equal(out, ident("df"))
})

test_that("connection affects SQL generation", {
  lf <- lazy_frame(x = 1, y = 5) %>% summarise(n = n())

  out1 <- lf %>% sql_build()
  out2 <- lf %>% sql_build(con = structure(list(), class = "PostgreSQLConnection"))

  expect_equal(out1$select, sql('COUNT() AS "n"'))
  expect_equal(out2$select, sql('count(*) AS "n"'))
})

# select and rename -------------------------------------------------------

test_that("select picks variables", {
  out <- lazy_frame(x1 = 1, x2 = 1, x3 = 2) %>%
    select(x1:x2) %>%
    sql_build()

  expect_equal(out$select, ident("x1" = "x1", "x2" = "x2"))
})

test_that("select renames variables", {
  out <- lazy_frame(x1 = 1, x2 = 1, x3 = 2) %>%
    select(y = x1, z = x2) %>%
    sql_build()

  expect_equal(out$select, ident("y" = "x1", "z" = "x2"))
})

test_that("select can refer to variables in local env", {
  vars <- c("x", "y")
  out <- lazy_frame(x = 1, y = 1) %>%
    select(one_of(vars)) %>%
    sql_build()

  expect_equal(out$select, ident("x" = "x", "y" = "y"))
})

test_that("rename preserves existing vars", {
  out <- lazy_frame(x = 1, y = 1) %>%
    rename(z = y) %>%
    sql_build()

  expect_equal(out$select, ident("x" = "x", "z" = "y"))
})


# arrange -----------------------------------------------------------------

test_that("arrange generates order_by", {
  out <- lazy_frame(x = 1, y = 1) %>%
    arrange(x) %>%
    sql_build()

  expect_equal(out$order_by, sql('"x"'))
})

test_that("arrange converts desc", {
  out <- lazy_frame(x = 1, y = 1) %>%
    arrange(desc(x)) %>%
    sql_build()

  expect_equal(out$order_by, sql('"x" DESC'))
})

test_that("grouped arrange doesn't order by groups", {
  out <- lazy_frame(x = 1, y = 1) %>%
    group_by(x) %>%
    arrange(y) %>%
    sql_build()

  expect_equal(out$order_by, sql('"y"'))
})


# summarise ---------------------------------------------------------------

test_that("summarise generates group_by and select", {
  out <- lazy_frame(g = 1) %>%
    group_by(x) %>%
    summarise(n = n()) %>%
    sql_build()

  expect_equal(out$group_by, sql('"x"'))
  expect_equal(out$select, sql('"x"', 'COUNT() AS "n"'))
})


# filter ------------------------------------------------------------------

test_that("filter generates simple expressions", {
  out <- lazy_frame(x = 1) %>%
    filter(x > 1L) %>%
    sql_build()

  expect_equal(out$where, sql('"x" > 1'))
})

# mutate ------------------------------------------------------------------

test_that("mutate generates simple expressions", {
  out <- lazy_frame(x = 1) %>%
    mutate(y = x + 1L) %>%
    sql_build()

  expect_equal(out$select, sql('"x"', '"x" + 1 AS "y"'))
})

# ungroup by --------------------------------------------------------------

test_that("ungroup drops PARTITION BY", {
  out <- lazy_frame(x = 1) %>%
    group_by(x) %>%
    ungroup() %>%
    mutate(x = rank(x)) %>%
    sql_build()
  expect_equal(out$select, sql('rank() OVER (ORDER BY "x") AS "x"'))

})

# distinct ----------------------------------------------------------------

test_that("distinct sets flagged", {
  out1 <- lazy_frame(x = 1) %>%
    select() %>%
    sql_build()
  expect_false(out1$distinct)

  out2 <- lazy_frame(x = 1) %>%
    distinct() %>%
    sql_build()
  expect_true(out2$distinct)
})


# head --------------------------------------------------------------------

test_that("head limits rows", {
  out <- lazy_frame(x = 1:100) %>%
    head(10) %>%
    sql_build()

  expect_equal(out$limit, 10)
})


# joins -------------------------------------------------------------------

test_that("join captures both tables", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  out <- inner_join(lf1, lf2) %>% sql_build()

  expect_equal(op_vars(out$x), c("x", "y"))
  expect_equal(op_vars(out$y), c("x", "z"))
  expect_equal(out$type, "inner")
})

test_that("semi join captures both tables", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  out <- semi_join(lf1, lf2) %>% sql_build()

  expect_equal(op_vars(out$x), c("x", "y"))
  expect_equal(op_vars(out$y), c("x", "z"))
  expect_equal(out$anti, FALSE)
})

test_that("set ops captures both tables", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  out <- union(lf1, lf2) %>% sql_build()

  expect_equal(op_vars(out$x), c("x", "y"))
  expect_equal(op_vars(out$y), c("x", "z"))
  expect_equal(out$type, "UNION")
})
