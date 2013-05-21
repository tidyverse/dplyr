context("Filter")

make_sql_table <- function(x, name = deparse(substitute(x)),
                           path = paste0(name, ".sqlite3"), ...) {
  stopifnot(require("RSQLite"))
  test_con <- dbConnect(dbDriver("SQLite"), dbname = path)
  on.exit(dbDisconnect(test_con))

  object <- as.data.frame(x)
  dbWriteTable(test_con, name, x, row.names = FALSE, overwrite = TRUE)

  source_sqlite(path, name)
}

strip <- function(x) {
  x <- as.data.frame(x)
  rownames(x) <- NULL
  x
}


df <- expand.grid(a = 1:10, b = letters[1:10],
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE)

sdf <- source_df(df)
sdt <- source_dt(df)
sdb <- make_sql_table(df)

test_that("filter results independent of data source (simple)", {
  sel <- c("d", "g", "a")
  expected <- strip(df[df$a > 6 & df$b %in% sel, , drop = FALSE])

  expect_equal(strip(filter(sdf, a > 6 & b %in% sel)), expected)
  expect_equal(strip(filter(sdt, a > 6 & b %in% sel)), expected)
  expect_equal(strip(filter(sdb, a > 6 & b %in% sel)), expected)
})
