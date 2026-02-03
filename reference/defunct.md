# Defunct functions

**\[defunct\]**

These functions were deprecated for at least two years before being made
defunct. If there's a known replacement, calling the function will tell
you about it.

## Usage

``` r
# Deprecated in 1.0.0 -------------------------------------

combine(...)

src_mysql(
  dbname,
  host = NULL,
  port = 0L,
  username = "root",
  password = "",
  ...
)

src_postgres(
  dbname = NULL,
  host = NULL,
  port = NULL,
  user = NULL,
  password = NULL,
  ...
)

src_sqlite(path, create = FALSE)

src_local(tbl, pkg = NULL, env = NULL)

src_df(pkg = NULL, env = NULL)

tbl_df(data)

as.tbl(x, ...)

add_rownames(df, var = "rowname")
```
