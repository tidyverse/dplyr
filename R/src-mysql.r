#' Connect to mysql/mariadb.
#'
#' Use \code{src_mysql} to connect to an existing mysql or mariadb database,
#' and \code{tbl} to connect to tables within that database.
#' If you are running a local mysqlql database, leave all parameters set as
#' their defaults to connect. If you're connecting to a remote database,
#' ask your database administrator for the values of these variables.
#'
#' @template db-info
#' @param dbname Database name
#' @param host,port Host name and port number of database
#' @param user,password User name and password. Rather than supplying a
#'   username and password here, it's better to save them in \code{my.cnf},
#'   as described in \code{\link[RMySQL]{MySQL}}. In that case, supply
#'   \code{NULL} to both \code{user} and \code{password}.
#' @param ... for the src, other arguments passed on to the underlying
#'   database connector, \code{dbConnect}. For the tbl, included for
#'   compatibility with the generic, but otherwise ignored.
#' @param src a mysql src created with \code{src_mysql}.
#' @param from Either a string giving the name of table in database, or
#'   \code{\link{sql}} described a derived table or compound join.
#' @export
#' @examples
#' \dontrun{
#' # Connection basics ---------------------------------------------------------
#' # To connect to a database first create a src:
#' my_db <- src_mysql(host = "blah.com", user = "hadley",
#'   password = "pass")
#' # Then reference a tbl within that src
#' my_tbl <- tbl(my_db, "my_table")
#' }
#'
#' # Here we'll use the Lahman database: to create your own local copy,
#' # create a local database called "lahman", or tell lahman_mysql() how to
#' # a database that you can write to
#'
#' if (has_lahman("mysql")) {
#' # Methods -------------------------------------------------------------------
#' batting <- tbl(lahman_mysql(), "Batting")
#' dim(batting)
#' colnames(batting)
#' head(batting)
#'
#' # Data manipulation verbs ---------------------------------------------------
#' filter(batting, yearID > 2005, G > 130)
#' select(batting, playerID:lgID)
#' arrange(batting, playerID, desc(yearID))
#' summarise(batting, G = mean(G), n = n())
#' mutate(batting, rbi2 = 1.0 * R / AB)
#'
#' # note that all operations are lazy: they don't do anything until you
#' # request the data, either by `print()`ing it (which shows the first ten
#' # rows), by looking at the `head()`, or `collect()` the results locally.
#'
#' system.time(recent <- filter(batting, yearID > 2010))
#' system.time(collect(recent))
#'
#' # Group by operations -------------------------------------------------------
#' # To perform operations by group, create a grouped object with group_by
#' players <- group_by(batting, playerID)
#' group_size(players)
#'
#' # MySQL doesn't support windowed functions, which means that only
#' # grouped summaries are really useful:
#' summarise(players, mean_g = mean(G), best_ab = max(AB))
#'
#' # When you group by multiple level, each summarise peels off one level
#' per_year <- group_by(batting, playerID, yearID)
#' stints <- summarise(per_year, stints = max(stint))
#' filter(ungroup(stints), stints > 3)
#' summarise(stints, max(stints))
#'
#' # Joins ---------------------------------------------------------------------
#' player_info <- select(tbl(lahman_mysql(), "Master"), playerID, hofID,
#'   birthYear)
#' hof <- select(filter(tbl(lahman_mysql(), "HallOfFame"), inducted == "Y"),
#'  hofID, votedBy, category)
#'
#' # Match players and their hall of fame data
#' inner_join(player_info, hof)
#' # Keep all players, match hof data where available
#' left_join(player_info, hof)
#' # Find only players in hof
#' semi_join(player_info, hof)
#' # Find players not in hof
#' anti_join(player_info, hof)
#'
#' # Arbitrary SQL -------------------------------------------------------------
#' # You can also provide sql as is, using the sql function:
#' batting2008 <- tbl(lahman_mysql(),
#'   sql("SELECT * FROM Batting WHERE YearID = 2008"))
#' batting2008
#' }
src_mysql <- function(dbname, host = NULL, port = 0L, user = "root",
                      password = "", ...) {
  if (!require("RMySQL")) {
    stop("RMySQL package required to connect to mysql/mariadb", call. = FALSE)
  }

  con <- DBI::dbConnect(MySQL(), dbname = dbname , host = host, port = port,
    username = user, password = password, ...)
  info <- DBI::dbGetInfo(con)

  src_sql("mysql", con,
    info = info, disco = db_disconnector(con, "mysql"))
}

#' @export
#' @rdname src_mysql
tbl.src_mysql <- function(src, from, ...) {
  tbl_sql("mysql", src = src, from = from, ...)
}

#' @export
brief_desc.src_mysql <- function(x) {
  info <- x$info

  paste0("mysql ", info$serverVersion, " [", info$user, "@",
    info$host, ":", info$port, "/", info$dbname, "]")
}

#' @export
translate_env.src_mysql <- function(x) {
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
db_has_table.MySQLConnection <- function(con, table) {
  # MySQL has no way to list temporary tables, so we always NA to
  # skip any local checks and rely on the database to throw informative errors
  NA
}

#' @export
db_data_type.MySQLConnection <- function(con, fields) {
  char_type <- function(x) {
    n <- max(nchar(as.character(x), "bytes"))
    if (n <= 65535) {
      paste0("varchar(", n, ")")
    } else {
      "mediumtext"
    }
  }

  data_type <- function(x) {
    switch(class(x)[1],
      logical = "boolean",
      integer = "integer",
      numeric = "double",
      factor =  char_type(x),
      character = char_type(x),
      Date =    "date",
      POSIXct = "datetime",
      stop("Unknown class ", paste(class(x), collapse = "/"), call. = FALSE)
    )
  }
  vapply(fields, data_type, character(1))
}

#' @export
sql_begin.MySQLConnection <- function(con) {
  dbGetQuery(con, "START TRANSACTION")
}

#' @export
sql_commit.MySQLConnection <- function(con) {
  dbGetQuery(con, "COMMIT")
}

#' @export
sql_insert_into.MySQLConnection <- function(con, table, values) {

  # Convert factors to strings
  is_factor <- vapply(values, is.factor, logical(1))
  values[is_factor] <- lapply(values[is_factor], as.character)

  # Encode special characters in strings
  is_char <- vapply(values, is.character, logical(1))
  values[is_char] <- lapply(values[is_char], encodeString)

  tmp <- tempfile(fileext = ".csv")
  write.table(values, tmp, sep = "\t", quote = FALSE, qmethod = "escape",
    row.names = FALSE, col.names = FALSE)

  sql <- build_sql("LOAD DATA LOCAL INFILE ", encodeString(tmp), " INTO TABLE ",
    ident(table), con = con)
  dbGetQuery(con, sql)

  invisible()
}


#' @export
sql_create_indexes.MySQLConnection <- function(con, table, indexes = NULL, ...) {
  sql_add_index <- function(columns) {
    name <- paste0(c(table, columns), collapse = "_")
    fields <- escape(ident(columns), parens = TRUE, con = con)
    build_sql("ADD INDEX ", ident(name), " ", fields, con = con)
  }
  add_index <- sql(vapply(indexes, sql_add_index, character(1)))
  sql <- build_sql("ALTER TABLE ", ident(table), "\n",
    escape(add_index, collapse = ",\n"), con = con)
  dbGetQuery(con, sql)
}


#' @export
sql_analyze.MySQLConnection <- function(con, table) {
  sql <- build_sql("ANALYZE TABLE", ident(table), con = con)
  dbGetQuery(con, sql)
}
