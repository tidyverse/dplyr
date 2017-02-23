#' Create a database table in temporary in-memory database.
#'
#' `memdb_frame()` works like [tibble::tibble()], but instead of creating a new
#' data frame in R, it creates a table in [src_memdb()].
#'
#' @export
#' @examples
#' if (require("RSQLite")) {
#' df <- memdb_frame(x = runif(100), y = runif(100))
#' df %>% arrange(x)
#' df %>% arrange(x) %>% show_query()
#' }
#'
#' @inheritParams tibble::data_frame
#' @param .name Name of table in database: defaults to a random name that's
#'   unlikely to conflict with an existing table.
#' @export
memdb_frame <- function(..., .name = random_table_name()) {
  copy_to(src_memdb(), data_frame(...), name = .name)
}

#' @export
src_desc.SQLiteConnection <- function(x) {
  paste0("sqlite ", sqlite_version(), " [", x@dbname, "]")
}

sqlite_version <- function() {
  if (utils::packageVersion("RSQLite") > 1) {
    RSQLite::rsqliteVersion()[[2]]
  } else {
    DBI::dbGetInfo(RSQLite::SQLite())$clientVersion
  }
}

# SQL methods -------------------------------------------------------------

#' @export
sql_translate_env.SQLiteConnection <- function(con) {
  sql_variant(
    sql_translator(.parent = base_scalar,
      log     = function(x, base = exp(1)) {
        if (base != exp(1)) {
          build_sql("log(", x, ") / log(", base, ")")
        } else {
          build_sql("log(", x, ")")
        }
      }
    ),
    sql_translator(.parent = base_agg,
      sd = sql_prefix("stdev")
    ),
    base_no_win
  )
}

#' @export
sql_escape_ident.SQLiteConnection <- function(con, x) {
  sql_quote(x, "`")
}

#' @export
sql_subquery.SQLiteConnection <- function(con, from, name = unique_name(), ...) {
  if (is.ident(from)) {
    setNames(from, name)
  } else {
    if (is.null(name)) {
      build_sql("(", from, ")", con = con)
    } else {
      build_sql("(", from, ") AS ", ident(name), con = con)
    }
  }
}


# DBI methods ------------------------------------------------------------------

#' @export
db_insert_into.SQLiteConnection <- function(con, table, values, ...) {
  DBI::dbWriteTable(con, table, values, append = TRUE, row.names = FALSE)
}
