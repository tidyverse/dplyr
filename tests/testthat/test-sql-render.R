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
