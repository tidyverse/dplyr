context("SQL: render")
# These test the full SQL rendering pipeline by running very simple examples
# against a live SQLite database.

# Single table ------------------------------------------------------------

test_that("rendering table wraps in SELECT *", {
  out <- memdb_frame(x = 1)
  expect_match(out %>% sql_render, "^SELECT [*]\nFROM `[^`]*`$")
  expect_equal(out %>% collect, data_frame(x = 1))
})

test_that("quoting for rendering mutated grouped table", {
  out <- memdb_frame(x = 1, y = 2) %>% mutate(y = x)
  expect_match(out %>% sql_render, "^SELECT `x`, `x` AS `y`\nFROM `[^`]*`$")
  expect_equal(out %>% collect, data_frame(x = 1, y = 1))
})

test_that("quoting for rendering ordered grouped table", {
  out <- memdb_frame(x = 1, y = 2) %>% group_by(x) %>% arrange(y)
  expect_match(out %>% sql_render, "^SELECT [*]\nFROM `[^`]*`\nORDER BY `y`$")
  expect_equal(out %>% collect, data_frame(x = 1, y = 2))
})

test_that("quoting for rendering summarized grouped table", {
  out <- memdb_frame(x = 1) %>% group_by(x) %>% summarize(n = n())
  expect_match(out %>% sql_render, "^SELECT `x`, COUNT[(][)] AS `n`\nFROM `[^`]*`\nGROUP BY `x`$")
  expect_equal(out %>% collect, data_frame(x = 1, n = 1L))
})

# Single table verbs ------------------------------------------------------

test_that("select quotes correctly", {
  out <- memdb_frame(x = 1, y = 1) %>%
    select(x) %>%
    collect()
  expect_equal(out, data_frame(x = 1))
})

test_that("select can rename", {
  out <- memdb_frame(x = 1, y = 2) %>%
    select(y = x) %>%
    collect()
  expect_equal(out, data_frame(y = 1))
})

test_that("distinct adds DISTINCT suffix", {
  out <- memdb_frame(x = c(1, 1)) %>% distinct()

  expect_match(out %>% sql_render(), "SELECT DISTINCT")
  expect_equal(out %>% collect(), data_frame(x = 1))
})

test_that("distinct over columns uses GROUP BY", {
  out <- memdb_frame(x = c(1, 2), y = c(1, 1)) %>% distinct(y)

  expect_match(out %>% sql_render(), "SELECT `y`.*GROUP BY `y`")
  expect_equal(out %>% collect(), data_frame(y = 1))
})

test_that("head limits rows returned", {
  out <- memdb_frame(x = 1:100) %>%
    head(10) %>%
    collect()

  expect_equal(nrow(out), 10)
})

test_that("head accepts fractional input", {
  out <- memdb_frame(x = 1:100) %>%
    head(10.5) %>%
    collect()

  expect_equal(nrow(out), 10)
})

test_that("head renders to integer fractional input", {
  out <- memdb_frame(x = 1:100) %>%
    head(10.5) %>%
    sql_render()

  expect_match(out, "LIMIT 10$")
})

test_that("head works with huge whole numbers", {
  out <- memdb_frame(x = 1:100) %>%
    head(1e10) %>%
    collect()

  expect_equal(out, data_frame(x = 1:100))
})

test_that("mutate overwrites previous variables", {
  df <- memdb_frame(x = 1:5) %>%
    mutate(x = x + 1) %>%
    mutate(x = x + 1) %>%
    collect()

  expect_equal(names(df), "x")
  expect_equal(df$x, 1:5 + 2)
})

test_that("sequence of operations work", {
  out <- memdb_frame(x = c(1, 2, 3, 4)) %>%
    select(y = x) %>%
    mutate(z = 2 * y) %>%
    filter(z == 2) %>%
    collect()

  expect_equal(out, data_frame(y = 1, z = 2))
})

test_that("compute creates correct column names", {
  out <- memdb_frame(x = 1) %>%
    group_by(x) %>%
    summarize(n = n()) %>%
    compute() %>%
    collect()

  expect_equal(out, data_frame(x = 1, n = 1L))
})

# Joins make valid sql ----------------------------------------------------

test_that("join generates correct sql", {
  lf1 <- memdb_frame(x = 1, y = 2)
  lf2 <- memdb_frame(x = 1, z = 3)

  out <- lf1 %>%
    inner_join(lf2, by = "x") %>%
    collect()

  expect_equal(out, data.frame(x = 1, y = 2, z = 3))
})

test_that("semi join generates correct sql", {
  lf1 <- memdb_frame(x = c(1, 2), y = c(2, 3))
  lf2 <- memdb_frame(x = 1)

  lf3 <- inner_join(lf1, lf2, by = "x")
  expect_equal(op_vars(lf3), c("x", "y"))

  out <- collect(lf3)
  expect_equal(out, data.frame(x = 1, y = 2))
})


test_that("set ops generates correct sql", {
  lf1 <- memdb_frame(x = 1)
  lf2 <- memdb_frame(x = c(1, 2))

  out <- lf1 %>%
    union(lf2) %>%
    collect()

  expect_equal(out, data.frame(x = c(1, 2)))
})
