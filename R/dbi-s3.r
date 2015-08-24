#' @import DBI
NULL


#' Source generics.
#'
#' These generics retrieve metadata for a given src.
#'
#' @keywords internal
#' @name backend_src
NULL

#' @name backend_src
#' @export
src_desc <- function(x) UseMethod("src_desc")

#' @name backend_src
#' @export
src_translate_env <- function(x) UseMethod("src_translate_env")


#' Database generics.
#'
#' These generics execute actions on the database. All generics have a method
#' for \code{DBIConnection} which typically just call the standard DBI S4
#' method.
#'
#' @section copy_to:
#' Currently, the only user of \code{sql_begin()}, \code{sql_commit()},
#' \code{sql_rollback()}, \code{sql_create_table()}, \code{sql_insert_into()},
#' \code{sql_create_indexes()}, \code{sql_drop_table()} and
#' \code{sql_analyze()}. If you find yourself overriding many of these
#' functions it may suggest that you should just override \code{\link{copy_to}}
#' instead.
#'
#' @return A logical value indicating success. Most failures should generate
#'  an error.
#' @name backend_db
#' @param con A database connection.
#' @keywords internal
NULL

#' @name backend_db
#' @export
db_create_database <- function(con, nameDB, owner = NULL) {
  UseMethod("db_create_database")
}
#' @name backend_db
#' @export
db_create_database.DBIConnection <- function(con, nameDB, owner = NULL) {
  
  assert_that(!is.null(con), is.character(nameDB), length(nameDB) == 1)
  
  if (is.null(owner)){
    s <- build_sql("CREATE DATABASE ", ident(nameDB), con = con)
  } else {
    s <- build_sql("CREATE DATABASE ", ident(nameDB), " OWNER ", ident(owner), con = con)
  }

  dbGetQuery(con, s)
  
}


#' @name backend_db
#' @export
db_list_tables <- function(con) UseMethod("db_list_tables")
#' @export
db_list_tables.DBIConnection <- function(con) dbListTables(con)

#' @name backend_db
#' @export
#' @param table A string, the table name.
db_has_table <- function(con, table) UseMethod("db_has_table")
#' @export
db_has_table.DBIConnection <- function(con, table) dbExistsTable(con, table)

#' @name backend_db
#' @export
#' @param fields A list of fields, as in a data frame.
db_data_type <- function(con, fields) UseMethod("db_data_type")
#' @export
db_data_type.DBIConnection <- function(con, fields) {
  vapply(fields, dbDataType, dbObj = con, FUN.VALUE = character(1))
}

#' @name backend_db
#' @export
db_save_query <- function(con, sql, name, temporary = TRUE, ...) {
  UseMethod("db_save_query")
}

#' @export
db_save_query.DBIConnection <- function(con, sql, name, temporary = TRUE,
                                        ...) {
  tt_sql <- build_sql("CREATE ", if (temporary) sql("TEMPORARY "),
    "TABLE ", ident(name), " AS ", sql, con = con)
  dbGetQuery(con, tt_sql)
  name
}

#' @name backend_db
#' @export
db_begin <- function(con, ...) UseMethod("db_begin")
#' @export
db_begin.DBIConnection <- function(con, ...) {
  dbBegin(con)
}

#' @name backend_db
#' @export
db_commit <- function(con, ...) UseMethod("db_commit")
#' @export
db_commit.DBIConnection <- function(con, ...) dbCommit(con)

#' @name backend_db
#' @export
db_rollback <- function(con, ...) UseMethod("db_rollback")
#' @export
db_rollback.DBIConnection <- function(con, ...) dbRollback(con)

#' @name backend_db
#' @export
db_create_table <- function(con, table, types, temporary = FALSE, ...) {
  UseMethod("db_create_table")
}
#' @export
db_create_table.DBIConnection <- function(con, table, types,
                                           temporary = FALSE, ...) {
  assert_that(is.string(table), is.character(types))

  field_names <- escape(ident(names(types)), collapse = NULL, con = con)
  fields <- sql_vector(paste0(field_names, " ", types), parens = TRUE,
    collapse = ", ", con = con)
  sql <- build_sql("CREATE ", if (temporary) sql("TEMPORARY "),
    "TABLE ", ident(table), " ", fields, con = con)

  dbGetQuery(con, sql)
}

#' @name backend_db
#' @export
db_insert_into <- function(con, table, values, ...) {
  UseMethod("db_insert_into")
}

db_create_indexes <- function(con, table, indexes = NULL, ...) {
  if (is.null(indexes)) return()
  assert_that(is.list(indexes))

  for(index in indexes) {
    db_create_index(con, table, index, ...)
  }
}

#' @name backend_db
#' @export
db_create_index <- function(con, table, columns, name = NULL, ...) {
  UseMethod("db_create_index")
}

#' @export
db_create_index.DBIConnection <- function(con, table, columns, name = NULL,
                                           ...) {
  assert_that(is.string(table), is.character(columns))

  name <- name %||% paste0(c(table, columns), collapse = "_")
  fields <- escape(ident(columns), parens = TRUE, con = con)
  sql <- build_sql("CREATE INDEX ", ident(name), " ON ", ident(table), " ", fields,
    con = con)

  dbGetQuery(con, sql)
}

#' @name backend_db
#' @export
db_drop_table <- function(con, table, force = FALSE, ...) {
  UseMethod("db_drop_table")
}
#' @export
db_drop_table.DBIConnection <- function(con, table, force = FALSE, ...) {
  sql <- build_sql("DROP TABLE ", if (force) sql("IF EXISTS "), ident(table),
    con = con)
  dbGetQuery(con, sql)
}

#' @name backend_db
#' @export
db_analyze <- function(con, table, ...) UseMethod("db_analyze")
#' @export
db_analyze.DBIConnection <- function(con, table, ...) {
  sql <- build_sql("ANALYZE ", ident(table), con = con)
  dbGetQuery(con, sql)
}

#' @export
#' @rdname backend_db
db_explain <- function(con, sql, ...) {
  UseMethod("db_explain")
}



# SQL generation --------------------------------------------------------------

#' SQL generation.
#'
#' These generics are used to run build various SQL queries.  Default methods
#' are provided for \code{DBIConnection}, but variations in SQL across
#' databases means that it's likely that a backend will require at least a
#' few methods.
#'
#' @return An SQL string.
#' @name backend_sql
#' @param con A database connection.
#' @keywords internal
NULL

#' @rdname backend_sql
#' @export
sql_select <- function(con, select, from, where = NULL, group_by = NULL,
  having = NULL, order_by = NULL, limit = NULL, offset = NULL, ...) {
  UseMethod("sql_select")
}
#' @export
sql_select.DBIConnection <- function(con, select, from, where = NULL,
                                     group_by = NULL, having = NULL,
                                     order_by = NULL, limit = NULL,
                                     offset = NULL, ...) {

  out <- vector("list", 8)
  names(out) <- c("select", "from", "where", "group_by", "having", "order_by",
    "limit", "offset")

  assert_that(is.character(select), length(select) > 0L)
  out$select <- build_sql("SELECT ", escape(select, collapse = ", ", con = con))

  assert_that(is.character(from), length(from) == 1L)
  out$from <- build_sql("FROM ", from, con = con)

  if (length(where) > 0L) {
    assert_that(is.character(where))
    out$where <- build_sql("WHERE ",
      escape(where, collapse = " AND ", con = con))
  }

  if (!is.null(group_by)) {
    assert_that(is.character(group_by), length(group_by) > 0L)
    out$group_by <- build_sql("GROUP BY ",
      escape(group_by, collapse = ", ", con = con))
  }

  if (!is.null(having)) {
    assert_that(is.character(having), length(having) == 1L)
    out$having <- build_sql("HAVING ",
      escape(having, collapse = ", ", con = con))
  }

  if (!is.null(order_by)) {
    assert_that(is.character(order_by), length(order_by) > 0L)
    out$order_by <- build_sql("ORDER BY ",
      escape(order_by, collapse = ", ", con = con))
  }

  if (!is.null(limit)) {
    assert_that(is.integer(limit), length(limit) == 1L)
    out$limit <- build_sql("LIMIT ", limit, con = con)
  }

  if (!is.null(offset)) {
    assert_that(is.integer(offset), length(offset) == 1L)
    out$offset <- build_sql("OFFSET ", offset, con = con)
  }

  escape(unname(compact(out)), collapse = "\n", parens = FALSE, con = con)
}

#' @export
#' @rdname backend_sql
sql_subquery <- function(con, sql, name = random_table_name(), ...) {
  UseMethod("sql_subquery")
}
#' @export
sql_subquery.DBIConnection <- function(con, sql, name = unique_name(), ...) {
  if (is.ident(sql)) return(sql)

  build_sql("(", sql, ") AS ", ident(name), con = con)
}

#' @rdname backend_sql
#' @export
sql_join <- function(con, x, y, type = "inner", by = NULL, ...) {
  UseMethod("sql_join")
}
#' @export
sql_join.DBIConnection <- function(con, x, y, type = "inner", by = NULL, ...) {
  join <- switch(type,
    left = sql("LEFT"),
    inner = sql("INNER"),
    right = sql("RIGHT"),
    full = sql("FULL"),
    stop("Unknown join type:", type, call. = FALSE)
  )

  by <- common_by(by, x, y)
  using <- all(by$x == by$y)

  # Ensure tables have unique names
  x_names <- auto_names(x$select)
  y_names <- auto_names(y$select)
  uniques <- unique_names(x_names, y_names, by$x[by$x == by$y])

  if (is.null(uniques)) {
    sel_vars <- c(x_names, y_names)
  } else {
    x <- update(x, select = setNames(x$select, uniques$x))
    y <- update(y, select = setNames(y$select, uniques$y))

    by$x <- unname(uniques$x[by$x])
    by$y <- unname(uniques$y[by$y])

    sel_vars <- unique(c(uniques$x, uniques$y))
  }

  if (using) {
    cond <- build_sql("USING ", lapply(by$x, ident), con = con)
  } else {
    on <- sql_vector(paste0(sql_escape_ident(con, by$x), " = ", sql_escape_ident(con, by$y)),
      collapse = " AND ", parens = TRUE)
    cond <- build_sql("ON ", on, con = con)
  }

  from <- build_sql(
    'SELECT * FROM ',
    sql_subquery(con, x$query$sql), "\n\n",
    join, " JOIN \n\n" ,
    sql_subquery(con, y$query$sql), "\n\n",
    cond, con = con
  )
  attr(from, "vars") <- lapply(sel_vars, as.name)

  from
}

#' @rdname backend_sql
#' @export
sql_semi_join <- function(con, x, y, anti = FALSE, by = NULL, ...) {
  UseMethod("sql_semi_join")
}
#' @export
sql_semi_join.DBIConnection <- function(con, x, y, anti = FALSE, by = NULL, ...) {
  by <- common_by(by, x, y)

  left <- escape(ident("_LEFT"), con = con)
  right <- escape(ident("_RIGHT"), con = con)
  on <- sql_vector(paste0(
    left, ".", sql_escape_ident(con, by$x), " = ", right, ".", sql_escape_ident(con, by$y)),
    collapse = " AND ", parens = TRUE)

  from <- build_sql(
    'SELECT * FROM ', sql_subquery(con, x$query$sql, "_LEFT"), '\n\n',
    'WHERE ', if (anti) sql('NOT '), 'EXISTS (\n',
    '  SELECT 1 FROM ', sql_subquery(con, y$query$sql, "_RIGHT"), '\n',
    '  WHERE ', on, ')'
  )
  attr(from, "vars") <- x$select
  from
}

#' @rdname backend_sql
#' @export
sql_set_op <- function(con, x, y, method) {
  UseMethod("sql_set_op")
}
#' @export
sql_set_op.DBIConnection <- function(con, x, y, method) {
  sql <- build_sql(
    x$query$sql,
    "\n", sql(method), "\n",
    y$query$sql
  )
  attr(sql, "vars") <- x$select
  sql
}

#' @rdname backend_sql
#' @export
sql_escape_string <- function(con, x) UseMethod("sql_escape_string")

#' @export
sql_escape_string.DBIConnection <- function(con, x) {
  sql_quote(x, "'")
}
#' @export
sql_escape_string.NULL <- sql_escape_string.DBIConnection

#' @rdname backend_sql
#' @export
sql_escape_ident <- function(con, x) UseMethod("sql_escape_ident")

#' @export
sql_escape_ident.DBIConnection <- function(con, x) {
  sql_quote(x, '"')
}
#' @export
sql_escape_ident.NULL <- sql_escape_ident.DBIConnection


#' @rdname backend_db
#' @export
db_query_fields <- function(con, sql, ...) {
  UseMethod("db_query_fields")
}
#' @export
db_query_fields.DBIConnection <- function(con, sql, ...) {
  fields <- build_sql("SELECT * FROM ", sql, " WHERE 0=1", con = con)

  qry <- dbSendQuery(con, fields)
  on.exit(dbClearResult(qry))

  dbListFields(qry)
}
#' @export
db_query_fields.PostgreSQLConnection <- function(con, sql, ...) {
  fields <- build_sql("SELECT * FROM ", sql, " WHERE 0=1", con = con)

  qry <- dbSendQuery(con, fields)
  on.exit(dbClearResult(qry))

  dbGetInfo(qry)$fieldDescription[[1]]$name
}

#' @rdname backend_db
#' @export
db_query_rows <- function(con, sql, ...) {
  UseMethod("db_query_rows")
}
#' @export
db_query_rows.DBIConnection <- function(con, sql, ...) {
  from <- sql_subquery(con, sql, "master")
  rows <- build_sql("SELECT count(*) FROM ", from, con = con)

  as.integer(dbGetQuery(con, rows)[[1]])
}

# Utility functions ------------------------------------------------------------

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}

# Creates an environment that disconnects the database when it's
# garbage collected
db_disconnector <- function(con, name, quiet = FALSE) {
  reg.finalizer(environment(), function(...) {
    if (!quiet) {
      message("Auto-disconnecting ", name, " connection ",
        "(", paste(con@Id, collapse = ", "), ")")
    }
    dbDisconnect(con)
  })
  environment()
}

res_warn_incomplete <- function(res) {
  if (dbHasCompleted(res)) return()

  rows <- formatC(dbGetRowCount(res), big.mark = ",")
  warning("Only first ", rows, " results retrieved. Use n = -1 to retrieve all.",
    call. = FALSE)
}

