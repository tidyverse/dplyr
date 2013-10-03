#' @section Debugging:
#'
#' To see exactly what SQL is being sent to the database, you can set option
#' \code{dplyr.show_sql} to true: \code{options(dplyr.show_sql = TRUE).}
#' If you're wondering why a particularly query is slow, it can be helpful
#' to see the query plan. You can do this by setting
#' \code{options(dplyr.explain_sql = TRUE)}.
#'
#' @section Grouping:
#'
#' Typically you will create a grouped data table is to call the \code{group_by}
#' method on a mysql tbl: this will take care of capturing
#' the unevalated expressions for you.
#'
#' For best performance, the database should have an index on the variables
#' that you are grouping by. Use \code{\link{explain_sql}} to check that
#' mysql is using the indexes that you expect.
