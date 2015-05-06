#' @section Debugging:
#'
#' To see exactly what SQL is being sent to the database, you see
#' \code{\link{show_query}} and \code{\link{explain}}.
#'
#' @section Grouping:
#'
#' Typically you will create a grouped data table is to call the \code{group_by}
#' method on a mysql tbl: this will take care of capturing
#' the unevalated expressions for you.
#'
#' For best performance, the database should have an index on the variables
#' that you are grouping by. Use \code{\link{explain}} to check that
#' the database is using the indexes that you expect.
#'
#' @section Output:
#'
#' All data manipulation on SQL tbls are lazy: they will not actually
#' run the query or retrieve the data unless you ask for it: they all return
#' a new \code{\link{tbl_sql}} object. Use \code{\link{compute}} to run the
#' query and save the results in a temporary in the database, or use
#' \code{\link{collect}} to retrieve the results to R.
#'
#' Note that \code{do} is not lazy since it must pull the data into R.
#' It returns a \code{\link{tbl_df}} or \code{\link{grouped_df}}, with one
#' column for each grouping variable, and one list column that contains the
#' results of the operation. \code{do} never simplifies its output.
#'
#' @section Query principles:
#'
#' This section attempts to lay out the principles governing the generation
#' of SQL queries from the manipulation verbs.  The basic principle is that
#' a sequence of operations should return the same value (modulo class)
#' regardless of where the data is stored.
#'
#' \itemize{
#'  \item \code{arrange(arrange(df, x), y)} should be equivalent to
#'    \code{arrange(df, y, x)}
#'
#'  \item \code{select(select(df, a:x), n:o)} should be equivalent to
#'    \code{select(df, n:o)}
#'
#'  \item \code{mutate(mutate(df, x2 = x * 2), y2 = y * 2)} should be
#'     equivalent to \code{mutate(df, x2 = x * 2, y2 = y * 2)}
#'
#'  \item \code{filter(filter(df, x == 1), y == 2)} should be
#'     equivalent to \code{filter(df, x == 1, y == 2)}
#'
#'  \item \code{summarise} should return the summarised output with
#'    one level of grouping peeled off.
#' }
NULL
