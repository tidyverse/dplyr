#' Copy a local data frame to a remote src.
#'
#' This uploads a local data frame into a remote data source, creating the
#' table definition as needed. Wherever possible, the new object will be
#' temporary, limited to the current connection to the source.
#'
#' @param dest remote data source
#' @param df local data frame
#' @param name name for new remote table.
#' @param ... other parameters passed to methods.
#' @return a \code{tbl} object in the remote source
#' @export
copy_to <- function(dest, df, name = deparse(substitute(df)), ...) {
  UseMethod("copy_to")
}

#' Copy a local data fram to a sqlite src.
#'
#' This standard method works for all sql sources.
#'
#' @export
#' @param types a character vector giving variable types to use for the columns.
#'    See \url{http://www.sqlite.org/datatype3.html} for available types.
#' @param temporary if \code{TRUE}, will create a temporary table that is
#'   local to this connection and will be automatically deleted when the
#'   connection expires
#' @param indexes a list of character vectors. Each element of the list
#'   will create a new index.
#' @param analyze if \code{TRUE} (the default), will automatically ANALYZE the
#'   new table so that the query optimiser has useful information.
#' @inheritParams copy_to
#' @return a sqlite \code{\link{tbl}} object
#' @examples
#' if (require("RSQLite") && require("RSQLite.extfuns")) {
#' db <- src_sqlite(tempfile(), create = TRUE)
#'
#' iris2 <- copy_to(db, iris)
#' mtcars$model <- rownames(mtcars)
#' mtcars2 <- copy_to(db, mtcars, indexes = list("model"))
#'
#' explain(filter(mtcars2, model == "Hornet 4 Drive"))
#'
#' # Note that tables are temporary by default, so they're not
#' # visible from other connections to the same database.
#' src_tbls(db)
#' db2 <- src_sqlite(db$path)
#' src_tbls(db2)
#' }
copy_to.src_sql <- function(dest, df, name = deparse(substitute(df)),
                            types = NULL, temporary = TRUE, indexes = NULL,
                            analyze = TRUE, ...) {
  assert_that(is.data.frame(df), is.string(name), is.flag(temporary))
  class(df) <- "data.frame" # avoid S4 dispatch problem in dbSendPreparedQuery

  if (isTRUE(db_has_table(dest$con, name))) {
    stop("Table ", name, " already exists.", call. = FALSE)
  }

  types <- types %||% db_data_type(dest$con, df)
  names(types) <- names(df)

  con <- dest$con
  db_begin(con)
  on.exit(db_rollback(con))

  db_create_table(con, name, types, temporary = temporary)
  db_insert_into(con, name, df)
  db_create_indexes(con, name, indexes)
  if (analyze) db_analyze(con, name)

  db_commit(con)
  on.exit(NULL)

  tbl(dest, name)
}

auto_copy <- function(x, y, copy = FALSE, ...) {
  if (same_src(x, y)) return(y)

  if (!copy) {
    stop("x and y don't share the same src. Set copy = TRUE to copy y into ",
      "x's source (this may be time consuming).", call. = FALSE)
  }

  UseMethod("auto_copy")
}

#' @export
auto_copy.tbl_sql <- function(x, y, copy = FALSE, ...) {
  copy_to(x$src, as.data.frame(y), random_table_name(), ...)
}

#' @export
auto_copy.tbl_dt <- function(x, y, copy = FALSE, ...) {
  data.table::as.data.table(as.data.frame(y))
}

#' @export
auto_copy.tbl_cube <- function(x, y, copy = FALSE, ...) {
  stop("Copying not supported by tbl_cube", call. = FALSE)
}

#' @export
auto_copy.tbl_df <- function(x, y, copy = FALSE, ...) {
  as.data.frame(y)
}
