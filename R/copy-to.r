#' Copy a local data frame to a remote src.
#' 
#' This uploads a local data frame into a remote data source, creating the
#' table definition as needed. Wherever possible, the new object will be
#' temporary, limited to the current connection to the source.
#' 
#' @param src remote data source
#' @param df local data frame
#' @param name name of remote table.
#' @param ... other parameters passed to methods.
#' @return a \code{tbl} object in the remote source
#' @export
copy_to <- function(dest, df, name = deparse(substitute(df)), ...) {
  UseMethod("copy_to")
}

#' Copy a local data fram to a sqlite src.
#' 
#' @method copy_to src_sql
#' @export
#' @param types a character vector giving variable types to use for the columns.
#'    See \url{http://www.sqlite.org/datatype3.html} for available types.
#' @param temporary if \code{TRUE}, will create a temporary table that is
#'   local to this connection and will be automatically deleted when the
#'   connection expires
#' 
#' @param indexes
#' @return a \code{\link{tbl_sqlite}} object
#' @examples
#' db <- src_sqlite(tempfile(), create = TRUE) 
#'
#' iris2 <- copy_to(db, iris)
#' mtcars$model <- rownames(mtcars)
#' mtcars2 <- copy_to(db, mtcars, indexes = list("model"))
#' 
#' explain_tbl(filter(mtcars2, model == "Hornet 4 Drive"))
#'
#' # Note that tables are temporary by default, so they're not 
#' # visible from other connections to the same database.
#' src_tbls(db)
#' db2 <- src_sqlite(db$path)
#' src_tbls(db2)
copy_to.src_sql <- function(dest, df, name = deparse(substitute(df)), 
                               types = NULL, temporary = TRUE, indexes = NULL, 
                               analyze = TRUE, ...) {
  assert_that(is.data.frame(df), is.string(name), is.flag(temporary))
  if (has_table(dest, name)) {
    stop("Table ", name, " already exists.", call. = FALSE)
  }

  types <- types %||% vapply(df, dbDataType, dbObj = dest$con, 
    FUN.VALUE = character(1))
  names(types) <- names(df)
  
  begin_transaction(dest)
  create_table(dest, name, types, temporary = temporary)
  insert_into(dest, name, df)
  create_indexes(dest, name, indexes)
  if (analyze) analyze(dest, name)
  commit_transaction(dest)
  
  tbl(dest, name)
}

create_table <- function(x, table, types, temporary = FALSE) {
  assert_that(is.string(table), is.character(types))
  
  field_names <- escape(ident(names(types)), collapse = NULL)
  fields <- sql_vector(paste0(field_names, " ", types), parens = TRUE, 
    collapse = ", ")
  sql <- build_sql("CREATE ", if (temporary) sql("TEMPORARY "), 
    "TABLE ", ident(table), " ", fields)
  
  query(x$con, sql)$run()
}

insert_into <- function(x, table, values) {
  UseMethod("insert_into")
}

insert_into.src_sqlite <- function(x, table, values) {
  params <- paste(rep("?", ncol(values)), collapse = ", ")
  sql <- build_sql("INSERT INTO ", table, " VALUES (", sql(params), ")")

  query(x$con, sql)$run(values)
}

insert_into.src_postgres <- function(x, table, values) {
  cols <- lapply(values, escape, collapse = NULL, parens = FALSE)
  col_mat <- matrix(unlist(cols, use.names = FALSE), nrow = nrow(values))
  
  rows <- apply(col_mat, 1, paste0, collapse = ", ")
  values <- paste0("(", rows, ")", collapse = "\n, ")
  
  sql <- build_sql("INSERT INTO ", ident(table), " VALUES ", sql(values))  
  query(x$con, sql)$run(in_transaction = FALSE)
}


create_indexes <- function(x, table, indexes = NULL, ...) {
  if (is.null(indexes)) return()
  assert_that(is.list(indexes))
  
  for(index in indexes) {
    create_index(x, table, index, ...)
  }
}

create_index <- function(x, table, columns, name = NULL, unique = FALSE) {
  assert_that(is.string(table), is.character(columns))
  
  name <- name %||% paste0(c(table, columns), collapse = "_")
  
  sql <- build_sql("CREATE ", if (unique) sql("UNIQUE "), "INDEX ", ident(name), 
    " ON ", ident(table), " ", escape(ident(columns), parens = TRUE))
  
  query(x$con, sql)$run()
}

drop_table <- function(x, table, force = FALSE) {
  sql <- build_sql("DROP TABLE ", if (force) sql("IF EXISTS "), ident(table))
  query(x$con, sql)$run()
}

analyze <- function(x, table) {
  sql <- build_sql("ANALYZE ", ident(table))
  query(x$con, sql)$run()
}

has_table <- function(src, table) {
  table %in% src_tbls(src)
}

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}

auto_copy <- function(x, y, copy = FALSE, ...) {
  if (same_src(x, y)) return(y)
  
  if (!copy) {
    stop("x and y don't share the same src. Set copy = TRUE to copy y into ", 
      "x's source (this may be time consuming).", call. = FALSE)
  }
  
  UseMethod("auto_copy")
} 

#' @S3method auto_copy tbl_sqlite
auto_copy.tbl_sql <- function(x, y, copy = FALSE, ...) {
  copy_to(x$src, as.data.frame(y), random_table_name(), ...)
}

#' @S3method auto_copy tbl_dt
auto_copy.tbl_dt <- function(x, y, copy = FALSE, ...) {
  as.data.table(as.data.frame(y))
}

#' @S3method auto_copy tbl_df
auto_copy.tbl_df <- function(x, y, copy = FALSE, ...) {
  as.data.frame(y)
}
