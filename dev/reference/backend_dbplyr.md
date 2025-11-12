# Database and SQL generics.

The `sql_` generics are used to build the different types of SQL
queries. The default implementations in dbplyr generates ANSI 92
compliant SQL. The `db_` generics execute actions on the database. The
default implementations in dbplyr typically just call the standard DBI
S4 method.

## Usage

``` r
db_desc(x)

sql_translate_env(con)

db_list_tables(con)

db_has_table(con, table)

db_data_type(con, fields)

db_save_query(con, sql, name, temporary = TRUE, ...)

db_begin(con, ...)

db_commit(con, ...)

db_rollback(con, ...)

db_write_table(con, table, types, values, temporary = FALSE, ...)

db_create_table(con, table, types, temporary = FALSE, ...)

db_insert_into(con, table, values, ...)

db_create_indexes(con, table, indexes = NULL, unique = FALSE, ...)

db_create_index(con, table, columns, name = NULL, unique = FALSE, ...)

db_drop_table(con, table, force = FALSE, ...)

db_analyze(con, table, ...)

db_explain(con, sql, ...)

db_query_fields(con, sql, ...)

db_query_rows(con, sql, ...)

sql_select(
  con,
  select,
  from,
  where = NULL,
  group_by = NULL,
  having = NULL,
  order_by = NULL,
  limit = NULL,
  distinct = FALSE,
  ...
)

sql_subquery(con, from, name = random_table_name(), ...)

sql_join(con, x, y, vars, type = "inner", by = NULL, ...)

sql_semi_join(con, x, y, anti = FALSE, by = NULL, ...)

sql_set_op(con, x, y, method)

sql_escape_string(con, x)

sql_escape_ident(con, x)
```

## Arguments

- con:

  A database connection.

- table:

  A string, the table name.

- fields:

  A list of fields, as in a data frame.

## Value

Usually a logical value indicating success. Most failures should
generate an error. However, `db_has_table()` should return `NA` if
temporary tables cannot be listed with
[`DBI::dbListTables()`](https://dbi.r-dbi.org/reference/dbListTables.html)
(due to backend API limitations for example). As a result, you methods
will rely on the backend to throw an error if a table exists when it
shouldn't.

## Details

A few backend methods do not call the standard DBI S4 methods including

- `db_data_type()`: Calls
  [`DBI::dbDataType()`](https://dbi.r-dbi.org/reference/dbDataType.html)
  for every field (e.g. data frame column) and returns a vector of
  corresponding SQL data types

- `db_save_query()`: Builds and executes a
  `CREATE [TEMPORARY] TABLE <table> ...` SQL command.

- `db_create_index()`: Builds and executes a
  `CREATE INDEX <name> ON <table>` SQL command.

- `db_drop_table()`: Builds and executes a
  `DROP TABLE [IF EXISTS] <table>` SQL command.

- `db_analyze()`: Builds and executes an `ANALYZE <table>` SQL command.

Currently,
[`copy_to()`](https://dplyr.tidyverse.org/dev/reference/copy_to.md) is
the only user of `db_begin()`, `db_commit()`, `db_rollback()`,
`db_write_table()`, `db_create_indexes()`, `db_drop_table()` and
`db_analyze()`. If you find yourself overriding many of these functions
it may suggest that you should just override
[`copy_to()`](https://dplyr.tidyverse.org/dev/reference/copy_to.md)
instead.

`db_create_table()` and `db_insert_into()` have been deprecated in
favour of `db_write_table()`.
