context("SQL: render")
# These test the full SQL rendering pipeline by running very simple examples
# against a live SQLite database.

test_that("rendering table wraps in SELECT *", {
  out <- memdb_frame(x = 1) %>% collect()
  expect_equal(out, data_frame(x = 1))
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
  out <- memdb_frame(x = c(1, 1)) %>%
    distinct() %>%
    collect()

  expect_equal(out, data_frame(x = 1))
})

test_that("sequence of operations work", {
  out <- memdb_frame(x = c(1, 2, 3, 4)) %>%
    select(y = x) %>%
    mutate(z = 2 * y) %>%
    filter(z == 2) %>%
    collect()

  expect_equal(out, data_frame(y = 1, z = 2))
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

  out <- lf1 %>%
    inner_join(lf2, by = "x") %>%
    collect()

  expect_equal(out, data.frame(x = 1, y = 2))
})
