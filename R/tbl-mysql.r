#' Create an mysql tbl.
#'
#' You can create a mysql tbl with all the  connection information table
#' or with an existing \code{\link{src_mysql}} object. You need to use
#' \code{src_mysql}  if you're working with multiple tables from the same
#' database so that they use the same connection, and so can perform joins etc.
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
#'
#' @param ...,src either \code{src_mysql} options or a \code{src_mysql}
#'   object
#' @param from Either a string giving the name of table in database, or
#'   \code{\link{sql}} described a derived table or compound join.
#' @param ... other arguments ignored, but needed for compatibility with
#'   generic.
#' @export
#' @examples
#' \dontrun{
#' # You can create from a path and a table name
#' my_tbl <- tbl_mysql(host = "blah.com", user = "hadley",
#'   password = "pass", from = "my_table")
#'
#' # Or (better) from a mysql src and a table name
#' my_db <- src_mysql(host = "blah.com", user = "hadley",
#'   password = "pass")
#' my_tbl <- tbl(my_db, "my_table")
#' }
#'
#' # Here we'll use the Lahman database: you can create a local copy yourself
#' # using cache_lahman()
#' \dontrun{
#' batting <- tbl(lahman_mysql(), "Batting")
#' dim(batting)
#' colnames(batting)
#' head(batting)
#'
#' # Perform operations "by group":
#' players <- group_by(batting, id)
#' group_size(players)
#'
#' # See examples of data manipulation operations in ?manip_mysql
#'
#' # You can also provide sql as is, using the sql function:
#' batting2008 <- tbl(lahman_sqlite(),
#'   sql("SELECT * FROM Batting WHERE YearID = 2008"))
#' batting2008
#' }
tbl_mysql <- function(..., from) {
  src <- src_mysql(...)
  tbl(src, from)
}

#' @method tbl src_mysql
#' @export
#' @rdname tbl_mysql
tbl.src_mysql <- function(src, from, ...) {
  assert_that(is.character(from), length(from) == 1)
  
  if (!is.sql(from)) {
    if (!db_has_table(src$con, from)) {
      stop("Table ", from, " not found in database ", src$path, call. = FALSE)
    }
    
    from <- ident(from)
  } else {
    from <- build_sql("(", from, ") AS ", ident(random_table_name()))
  }
  
  tbl <- tbl_sql("mysql", src = src, from = from)
  tbl$summarise <- FALSE
  update(tbl,
    select = list(star()),
    where = NULL,
    group_by = NULL,
    order_by = NULL
  )
}
