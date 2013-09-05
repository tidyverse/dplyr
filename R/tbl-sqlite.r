#' Create an sqlite tbl.
#'
#' You can create a sqlite tbl with a table name and a path or 
#' \code{\link{src_sqlite}} object. You need to use \code{src_sqlite} 
#' if you're working with multiple tables from the same database so that they
#' use the same connection, and so can perform joins etc.
#'
#' To see exactly what SQL is being sent to the database, you can set option
#' \code{dplyr.show_sql} to true: \code{options(dplyr.show_sql = TRUE).}
#' If you're wondering why a particularly query is slow, it can be helpful
#' to see the query plan. You can do this by setting
#' \code{options(dplyr.explain_sql = TRUE)}. The output of SQLite's query
#' plans is relatively easy to make sense of and is explained at
#' \url{http://www.sqlite.org/eqp.html}. You may also find the explanation of
#' how SQL indices works to be helpful:
#' \url{http://www.sqlite.org/queryplanner.html}.
#'
#' @section Grouping:
#' 
#' Typically you will create a grouped data table is to call the \code{group_by}
#' method on a sqlite tbl: this will take care of capturing
#' the unevalated expressions for you.
#'
#' For best performance, the database should have an index on the variables
#' that you are grouping by. A good introduction to how indices affect database
#' performance can be found at \url{http://www.sqlite.org/queryplanner.html}.
#' See \code{\link{explain_sql}} to check that sqlite is using the indexes that
#' you expect.
#'
#' @param path,src either path to sqlite database, or \code{src_sqlite} object
#' @param table name of table in database. This can either be a string 
#'   representing a table name, or an \code{\link{sql}} string, representing
#'   a select query or compound join.
#' @param ... other arguments ignored, but needed for compatibility with 
#'   generic.
#' @export
#' @examples
#' # You can create from a path and a table name
#' db_path <- system.file("db", "baseball.sqlite3", package = "dplyr")
#' baseball_s <- tbl_sqlite(db_path, "baseball")
#' 
#' # Or (better) from a sqlite src and a table name
#' db <- src_sqlite(db_path)
#' baseball_s <- tbl(db, "baseball")
#' 
#' dim(baseball_s)
#' colnames(baseball_s)
#' head(baseball_s)
#'
#' # Perform operations "by group":
#' players <- group_by(baseball_s, id)
#' summarise(players, g = mean(g), n = count())
#'
#' by_year_lg <- group_by(baseball_s, year, lg)
#' group_size(by_year_lg)
#' summarise(by_year_lg, players = count(), avg_g = mean(g))
#'
#' by_team <- group_by(baseball_s, team)
#' summarise(by_team, players = count())
#'
#' # do by
#' mods <- do(by_team, failwith(NULL, lm), formula = r ~ poly(year, 2), .chunk_size = 1000)
#'
#' sizes <- summarise(by_team, freq = count())
#' not_small <- as.data.frame(filter(sizes, freq > 10))
#' teams <- not_small$team
#' ok <- filter(by_team, team %in% teams)
#' mods <- do(ok, failwith(NULL, lm), formula = r ~ poly(year, 2),
#'   .chunk_size = 1000)
#'
#' # Since we it's not easy to figure out what variables you are using
#' # in general, it will often be faster to let dplyr know what you need
#' mods <- do(filter(ok, year, r), failwith(NULL, lm), formula = r ~ poly(year, 2),
#'   .chunk_size = 1000)
tbl_sqlite <- function(path, table) {
  src <- src_sqlite(path)
  tbl(src, table)
}

#' @method tbl src_sqlite
#' @export
#' @rdname tbl_sqlite
tbl.src_sqlite <- function(src, table, ...) {
  if (!is.sql(table)) {
    if (!has_table(src, table)) {
      stop("Table ", table, " not found in database ", path, call. = FALSE)
    }
    
    table <- ident(table)
  }
  
  tbl_sql(c("sqlite_table", "sqlite"), 
    src = src, 
    table = table,
    select = NULL,
    filter = NULL,
    arrange = NULL,
    group_by = NULL
  )
}

#' @S3method same_src tbl_sqlite
same_src.tbl_sqlite <- function(x, y) {
  if (!inherits(y, "tbl_sqlite")) return(FALSE)
  same_src(x$src, y$src)
}

#' @S3method tbl_vars tbl_sqlite
tbl_vars.tbl_sqlite <- function(x) {
  qry_select(x)$vars()
}

# Grouping methods -------------------------------------------------------------

#' @S3method group_by tbl_sqlite
group_by.tbl_sqlite <- function(x, ...) {
  group_by <- partial_eval(named_dots(...), x, parent.frame())
  
  x$group_by <- c(x$group_by, group_by)
  x
}

#' @S3method group_size tbl_sqlite
group_size.tbl_sqlite <- function(x) {
  df <- collect(summarise(x, n()))
  df[[length(df)]]
}

# Standard data frame methods --------------------------------------------------

#' @S3method as.data.frame tbl_sqlite
as.data.frame.tbl_sqlite <- function(x, row.names = NULL, optional = NULL,
                                        ..., n = 1e5L) {
  qry_select(x)$fetch_df(n)
}

#' @S3method print tbl_sqlite
print.tbl_sqlite <- function(x, ...) {
  cat("Source: SQLite [", x$src$path, "]\n", sep = "")
  
  cat(wrap("From: ", gsub("\n", " ", x$table), " ", dim_desc(x)))
  cat("\n")
  if (!is.null(x$filter)) {
    cat(wrap("Filter: ", commas(deparse_all(x$filter))), "\n")
  }
  if (!is.null(x$arrange)) {
    cat(wrap("Arrange: ", commas(deparse_all(x$arrange))), "\n")
  }
  if (!is.null(x$group_by)) {
    cat(wrap("Grouped by: ", commas(deparse_all(x$group_by))), "\n")
  }
  
  if (!inherits(x$table, "ident")) return()
  
  cat("\n")
  trunc_mat(x)
}

#' @S3method dimnames tbl_sqlite
dimnames.tbl_sqlite <- function(x) {
  list(NULL, tbl_vars.tbl_sqlite(x))
}

#' @S3method dim tbl_sqlite
dim.tbl_sqlite <- function(x) {
  if (!inherits(x$table, "ident")) {
    n <- NA
  } else {
    n <- qry_select(x)$nrow()
  }
  
  p <- qry_select(x)$ncol()
  c(n, p)
}

#' @S3method head tbl_sqlite
head.tbl_sqlite <- function(x, n = 6L, ...) {
  assert_that(length(n) == 1, n > 0L)

  qry_select(x, limit = n)$fetch_df()
}

#' @S3method tail tbl_sqlite
tail.tbl_sqlite <- function(x, n = 6L, ...) {
  stop("tail is not supported by sqlite", call. = FALSE)
}
