#' @export
src_desc.MySQLConnection <- function(x) {
  info <- dbGetInfo(x)

  paste0(
    "mysql ", info$serverVersion, " [",
    info$user, "@", info$host, ":", info$port, "/", info$dbname,
    "]"
  )
}

#' @export
sql_translate_env.MySQLConnection <- function(con) {
  sql_variant(
    base_scalar,
    sql_translator(.parent = base_agg,
      n = function() sql("count(*)"),
      sd =  sql_prefix("stddev_samp"),
      var = sql_prefix("var_samp"),
      paste = function(x, collapse) build_sql("group_concat(", x, collapse, ")")
    )
  )
}

# DBI methods ------------------------------------------------------------------

#' @export
db_has_table.MySQLConnection <- function(con, table, ...) {
  # MySQL has no way to list temporary tables, so we always NA to
  # skip any local checks and rely on the database to throw informative errors
  NA
}

#' @export
db_data_type.MySQLConnection <- function(con, fields, ...) {
  char_type <- function(x) {
    n <- max(nchar(as.character(x), "bytes"), 0L, na.rm = TRUE)
    if (n <= 65535) {
      paste0("varchar(", n, ")")
    } else {
      "mediumtext"
    }
  }

  data_type <- function(x) {
    switch(
      class(x)[1],
      logical =   "boolean",
      integer =   "integer",
      numeric =   "double",
      factor =    char_type(x),
      character = char_type(x),
      Date =      "date",
      POSIXct =   "datetime",
      stop("Unknown class ", paste(class(x), collapse = "/"), call. = FALSE)
    )
  }
  vapply(fields, data_type, character(1))
}

#' @export
db_begin.MySQLConnection <- function(con, ...) {
  dbExecute(con, "START TRANSACTION")
}

#' @export
db_commit.MySQLConnection <- function(con, ...) {
  dbExecute(con, "COMMIT")
}

#' @export
db_rollback.MySQLConnection <- function(con, ...) {
  dbExecute(con, "ROLLBACK")
}

#' @export
db_insert_into.MySQLConnection <- function(con, table, values, ...) {

  # Convert factors to strings
  is_factor <- vapply(values, is.factor, logical(1))
  values[is_factor] <- lapply(values[is_factor], as.character)

  # Encode special characters in strings
  is_char <- vapply(values, is.character, logical(1))
  values[is_char] <- lapply(values[is_char], encodeString, na.encode = FALSE)

  tmp <- tempfile(fileext = ".csv")
  utils::write.table(values, tmp, sep = "\t", quote = FALSE, qmethod = "escape",
    na = "\\N", row.names = FALSE, col.names = FALSE)

  sql <- build_sql("LOAD DATA LOCAL INFILE ", encodeString(tmp), " INTO TABLE ",
    ident(table), con = con)
  dbExecute(con, sql)

  invisible()
}

#' @export
db_create_index.MySQLConnection <- function(con, table, columns, name = NULL,
                                            unique = FALSE, ...) {
  name <- name %||% paste0(c(table, columns), collapse = "_")
  fields <- escape(ident(columns), parens = TRUE, con = con)
  index <- build_sql(
    "ADD ",
    if (unique) sql("UNIQUE "),
    "INDEX ", ident(name), " ", fields,
    con = con
  )

  sql <- build_sql("ALTER TABLE ", ident(table), "\n", index, con = con)
  dbExecute(con, sql)
}

#' @export
db_analyze.MySQLConnection <- function(con, table, ...) {
  sql <- build_sql("ANALYZE TABLE", ident(table), con = con)
  dbExecute(con, sql)
}

#' @export
sql_escape_ident.MySQLConnection <- function(con, x) {
  sql_quote(x, "`")
}
