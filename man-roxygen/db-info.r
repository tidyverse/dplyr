#' @section Debugging:
#'
#' To see exactly what SQL is being sent to the database, you see
#' [show_query()] and [explain()].
#'
#' @section Grouping:
#'
#' Typically you will create a grouped data table is to call the [group_by()]
#' method on a database tbl: this will take care of capturing
#' the unevalated expressions for you.
#'
#' For best performance, the database should have an index on the variables
#' that you are grouping by. Use [explain()] to check that
#' the database is using the indexes that you expect.
#'
#' @section Output:
#'
#' All data manipulation on SQL tbls are lazy: they will not actually
#' run the query or retrieve the data unless you ask for it: they all return
#' a new [tbl_sql()] object. Use [compute()] to run the
#' query and save the results in a temporary in the database, or use
#' [collect()] to retrieve the results to R.
#'
#' Note that `do()` is not lazy since it must pull the data into R.
#' It returns a [tbl_df()] or [grouped_df()], with one
#' column for each grouping variable, and one list column that contains the
#' results of the operation. `do()` never simplifies its output.
#'
#' @section Query principles:
#'
#' This section attempts to lay out the principles governing the generation
#' of SQL queries from the manipulation verbs.  The basic principle is that
#' a sequence of operations should return the same value (modulo class)
#' regardless of where the data is stored.
#'
#' \itemize{
#'  \item `arrange(arrange(df, x), y)` should be equivalent to
#'    `arrange(df, y, x)`
#'
#'  \item `select(select(df, a:x), n:o)` should be equivalent to
#'    `select(df, n:o)`
#'
#'  \item `mutate(mutate(df, x2 = x * 2), y2 = y * 2)` should be
#'     equivalent to `mutate(df, x2 = x * 2, y2 = y * 2)`
#'
#'  \item `filter(filter(df, x == 1), y == 2)` should be
#'     equivalent to `filter(df, x == 1, y == 2)`
#'
#'  \item `summarise()` should return the summarised output with
#'    one level of grouping peeled off.
#' }
NULL
