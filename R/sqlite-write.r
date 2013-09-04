# i <- src_sqlite(tempfile(), create = TRUE) 
#
# remove_table(i, "iris", TRUE)
# types <- sapply(iris, dbDataType, dbObj = i$con)
# ir <- create_table(i, "iris", types)
# append_rows(ir, iris)
# remove_table(i, "iris")
#
# write_table(i, "iris", iris)
# remove_table(i, "iris")
# iris <- write_table(i, "iris", iris, temporary = TRUE)
# src_tbls(i)
#
# i2 <- src_sqlite(i$path)
# src_tbls(i2)

#' Create a new \code{tbl_sqlite} from a data frame
#' 
#' This uploads a local data frame into a remote data source, creating the
#' table definition as needed.
#' 
#' @param src SQLite data source, to which you have write access
#' @param table name of table to create. 
#' @param value a local data frame
#' @param temporary if \code{TRUE}, will create a temporary table that is
#'   local to this connection and will be automatically deleted when the
#'   connection expires
#' @return a \code{\link{tbl_sqlite}} object
#' @export
write_table <- function(src, table, value, types = NULL, temporary = FALSE) {
  types <- types %||% vapply(value, dbDataType, dbObj = src$con, 
    FUN.VALUE = character(1))
  
  tbl <- create_table(src, table, types = types, temporary = temporary)
  append_rows(tbl, value)
  
  tbl
}

create_table <- function(src, table, types, temporary = FALSE) {
  assert_that(is.string(table), is.character(types))
  if (has_table(src, table)) {
    stop("Table ", table, " already exists.", call. = FALSE)
  }
  
  # Generate and execute sql
  field_names <- escape(ident(names(types)), collapse = NULL)
  fields <- sql_vector(paste0(field_names, " ", types), parens = TRUE, 
    collapse = ", ")
  sql <- build_sql("CREATE ", if (temporary) sql("TEMPORARY "), "TABLE ", table, 
    fields)
  
  exec_sql(src$con, sql, fetch = FALSE, show = getOption("dplyr.show_sql"))
  
  # Return table object
  tbl(src, table)
}

create_index <- function(tbl, columns, name = NULL, unique = FALSE) {
  name <- name %||% paste0(c(tbl$table, columns), collapse = "_")

  sql <- build_sql("CREATE ", if (unique) sql("UNIQUE "), "INDEX ", ident(name), 
    " ON ", tbl$table, " ", escape(ident(columns), parens = TRUE))
  
  exec_sql(tbl$src$con, sql, fetch = FALSE, show = getOption("dplyr.show_sql"))
  
  TRUE
}

remove_table <- function(src, table, force = FALSE) {
  if (!has_table(src, table)) {
    if (force) {
      return(FALSE)
    } else {
      stop("Table ", table, " does not exist.", call. = FALSE)  
    }
  }
  
  dbRemoveTable(src$con, table)
  TRUE
}

append_rows <- function(tbl, values) {
  if (!identical(tbl_vars(tbl), names(values))) {
    stop("Variable names are not compatible", call. = FALSE)
  }
  
  params <- paste(rep("?", ncol(values)), collapse = ", ")
  sql <- build_sql("INSERT INTO ", tbl$table, " VALUES (", sql(params), ")")
  if (getOption("dplyr.show_sql")) {
    message(sql)
  }
  
  dbBeginTransaction(tbl$src$con)
  qry <- dbSendPreparedQuery(tbl$src$con, sql, bind.data = values)
  dbClearResult(qry)
  dbCommit(tbl$src$con)
  
  tbl
}

has_table <- function(src, table) {
  table %in% src_tbls(src)
}

