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
#' group_size(players)
#' 
#' # See examples of data manipulation operations in ?manip_sqlite
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
  
  tbl <- tbl_sql("sqlite", src = src, table = table)
  update(tbl,
    select = NULL,
    where = NULL,
    group_by = NULL,
    order_by = NULL
  )
}

is_table <- function(x) {
  if (!inherits(x$table, "ident")) return(FALSE)
  
  is.null(x$select) && is.null(x$where) && is.null(x$group_by) && 
    is.null(x$order_by)
}

#' @S3method update tbl_sqlite
update.tbl_sqlite <- function(object, ...) {
  args <- list(...)

  bad <- setdiff(names(list(...)), c("select", "where", "group_by", "order_by"))
  if (length(bad) > 0) {
    stop("Incorrect component names: ", paste0(bad, collapse = ", "), 
      call. = FALSE)
  }
  
  for (nm in names(args)) {
    object[[nm]] <- args[[nm]]
  }
  object$query <- qry_select(object)
  
  object
} 

#' @S3method same_src tbl_sqlite
same_src.tbl_sqlite <- function(x, y) {
  if (!inherits(y, "tbl_sqlite")) return(FALSE)
  same_src(x$src, y$src)
}

#' @S3method tbl_vars tbl_sqlite
tbl_vars.tbl_sqlite <- function(x) {
  x$query$vars()
}

#' @S3method groups tbl_sqlite
groups.tbl_sqlite <- function(x) {
  x$group_by
}

# Grouping methods -------------------------------------------------------------

#' @S3method group_by tbl_sqlite
group_by.tbl_sqlite <- function(x, ...) {
  group_by <- partial_eval(named_dots(...), x, parent.frame())
  
  update(x, group_by = c(x$group_by, group_by))
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
  x$query$fetch_df(n)
}

#' @S3method print tbl_sqlite
print.tbl_sqlite <- function(x, ...) {
  cat("Source: SQLite [", x$src$path, "]\n", sep = "")
  
  if (inherits(x$table, "ident")) {
    cat(wrap("From: ", x$table, " ", dim_desc(x)))
  } else {
    cat(wrap("From: <derived table> ", dim_desc(x)))    
  }
  cat("\n")
  if (!is.null(x$where)) {
    cat(wrap("Filter: ", commas(deparse_all(x$where))), "\n")
  }
  if (!is.null(x$order_by)) {
    cat(wrap("Arrange: ", commas(deparse_all(x$order_by))), "\n")
  }
  if (!is.null(x$group_by)) {
    cat(wrap("Grouped by: ", commas(deparse_all(x$group_by))), "\n")
  }
  
  cat("\n")
  
  rows <- nrow(x)
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
    n <- x$query$nrow()
  }
  
  p <- x$query$ncol()
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
