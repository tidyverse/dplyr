#' Create a "sql src" object
#'
#' \code{src_sql} is the standard constructor for all SLQ based srcs.
#'
#' @keywords internal
#' @export
#' @param subclass name of subclass. "src_sql" is an abstract base class, so you 
#'   must supply this value. \code{src_} is automatically prepended to the 
#'   class name
#' @param the connection object
#' @param ... fields used by object
src_sql <- function(subclass, con, ...) {
  subclass <- paste0("src_", subclass)
  structure(list(con = con, ...), class = c(subclass, "src_sql", "src"))
}

#' @S3method same_src src_sql
same_src.src_sql <- function(x, y) {
  if (!inherits(y, "src_sql")) return(FALSE)
  identical(x$con, y$con)
}

#' @S3method src_tbls src_sql
src_tbls.src_sql <- function(x, ...) {
  dbListTables(x$con)
}

#' @S3method format src_sql
format.src_sql <- function(x, ...) {
  paste0("src:  ", brief_desc(x), "\n",
    wrap("tbls: ", paste0(sort(src_tbls(x)), collapse = ", ")))
}

begin_transaction <- function(x) UseMethod("begin_transaction")
begin_transaction.src_sqlite <- function(x) dbBeginTransaction(x$con)
begin_transaction.src_sql <- function(x) {
  dbGetQuery(x$con, "BEGIN TRANSACTION")
}

commit_transaction <- function(x) UseMethod("commit_transaction")
commit_transaction.src_sql <- function(x) {
  dbCommit(x$con)
}

rollback_transaction <- function(x) UseMethod("rollback_transaction")
rollback_transaction <- function(x) {
  dbRollback(x$con)
}

in_transaction <- function(x, code) {
  begin_transaction(x)
  tryCatch(res <- code,
    error = function(e) {
      rollback_transack(x)
      stop(e)
    }
  )
  commit_transaction(x)
  res
}
