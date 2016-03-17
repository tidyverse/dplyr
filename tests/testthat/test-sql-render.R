context("SQL: render")

test_that("rendering table wraps in SELECT *", {
  out <- lazy_frame(x = 1) %>% sql_render()
  expect_equal(out, sql('SELECT *\nFROM "df"'))
})

test_that("select quotes correctly", {
  out <- lazy_frame(x = 1) %>%
    select(x = x) %>%
    sql_render()

  expect_equal(out, sql('SELECT "x" AS "x"\nFROM "df"'))
})

test_that("distinct adds DISTINCT suffix", {
  out <- lazy_frame(x = 1) %>% distinct() %>% sql_render()
  expect_equal(out, sql('SELECT DISTINCT *\nFROM "df"'))
})


# Joins make valid sql ----------------------------------------------------

test_that("join generates correct sql", {
  lf1 <- lazy_frame(x = 1, y = 2)
  lf2 <- lazy_frame(x = 1, z = 2)

  out <- inner_join(lf1, lf2, by = "x") %>% sql_render()
  expect_equal(out, sql('SELECT * FROM "df"\n\nINNER JOIN\n\n"df"\n\nUSING ("x")'))
})
