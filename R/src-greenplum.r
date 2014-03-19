#' Connect to greenplum.
#'
#' Use \code{src_greenplum} to connect to an existing greenplum database,
#' and \code{tbl} to connect to tables within that database.
#' If you are running a local greenplum database, leave all parameters set as
#' their defaults to connect. If you're connecting to a remote database,
#' ask your database administrator for the values of these variables.
#'
#' @template db-info
#' @param dbname Database name
#' @param host,port Host name and port number of database
#' @param user,password User name and password (if needed)
#' @param ... for the src, other arguments passed on to the underlying
#'   database connector, \code{dbConnect}. For the tbl, included for
#'   compatibility with the generic, but otherwise ignored.
#' @param src a greenplum src created with \code{src_greenplum}.
#' @param from Either a string giving the name of table in database, or
#'   \code{\link{sql}} described a derived table or compound join.
#' @export
#' @examples
#' \dontrun{
#' # Connection basics ---------------------------------------------------------
#' # To connect to a database first create a src:
#' my_db <- src_greenplum(host = "blah.com", user = "hadley",
#'   password = "pass")
#' # Then reference a tbl within that src
#' my_tbl <- tbl(my_db, "my_table")
#' }
#'
#' # Here we'll use the Lahman database: to create your own local copy,
#' # create a local database called "lahman", or tell lahman_greenplum() how to
#' # a database that you can write to
#'
#' if (has_lahman("greenplum")) {
#' # Methods -------------------------------------------------------------------
#' batting <- tbl(lahman_greenplum(), "Batting")
#' dim(batting)
#' colnames(batting)
#' head(batting)
#'
#' # Data manipulation verbs ---------------------------------------------------
#' filter(batting, yearID > 2005, G > 130)
#' select(batting, playerID:lgID)
#' arrange(batting, playerID, desc(yearID))
#' summarise(batting, G = mean(G), n = n())
#' mutate(batting, rbi2 = if(is.null(AB)) 1.0 * R / AB else 0)
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
#' summarise(players, mean_g = mean(G), best_ab = max(AB))
#' best_year <- filter(players, AB == max(AB) | G == max(G))
#' progress <- mutate(players, cyear = yearID - min(yearID) + 1,
#'  rank(desc(AB)), cumsum(AB, yearID))
#'
#' # When you group by multiple level, each summarise peels off one level
#' per_year <- group_by(batting, playerID, yearID)
#' stints <- summarise(per_year, stints = max(stint))
#' filter(stints, stints > 3)
#' summarise(stints, max(stints))
#' mutate(stints, cumsum(stints, yearID))
#'
#' # Joins ---------------------------------------------------------------------
#' player_info <- select(tbl(lahman_greenplum(), "Master"), playerID, hofID,
#'   birthYear)
#' hof <- select(filter(tbl(lahman_greenplum(), "HallOfFame"), inducted == "Y"),
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
#' batting2008 <- tbl(lahman_greenplum(),
#'   sql('SELECT * FROM "Batting" WHERE "yearID" = 2008'))
#' batting2008
#' }
src_greenplum <- function(dbname = NULL, host = NULL, port = NULL, user = NULL,
                         password = NULL, ...) {
  if (!require("RPostgreSQL")) {
    stop("RPostgreSQL package required to connect to greenplum db", call. = FALSE)
  }
  
  user <- user %||% if (in_travis()) "greenplum" else ""
  
  con <- dbi_connect(PostgreSQL(), host = host %||% "", dbname = dbname %||% "",
                     user = user, password = password %||% "", port = port %||% "", ...)
  info <- db_info(con)
  
  src_sql("greenplum", con,
          info = info, disco = db_disconnector(con, "greenplum"))
}

over_greenplum <- function(expr, partition = NULL, order = NULL, frame = NULL) {
  args <- (!is.null(partition)) + (!is.null(order)) + (!is.null(frame))
  if (args == 0) {
    stop("Must supply at least one of partition, order, frame", call. = FALSE)
  }
  
  if (!is.null(partition)) {
    partition <- build_sql("PARTITION BY ", 
                           sql_vector(partition, collapse = ", ", parens = FALSE))
  }
  if (!is.null(order)) {
    order <- build_sql("ORDER BY ", sql_vector(order, collapse = ", ", parens = FALSE))
  }
  if (!is.null(frame)) {
    if (is.numeric(frame)) frame <- rows(frame[1], frame[2])
    frame <- build_sql("ROWS ", frame)
  }
  
  over <- sql_vector(compact(list(partition, order, frame)), parens = TRUE)
  build_sql(expr, " OVER ", over)
}

win_recycled_greenplum <- function(f) {
  force(f)
  function(x) {
    over_greenplum(build_sql(sql(f), list(x)), partition_group(), NULL, frame = NULL)
  }
}

#' @export
#' @rdname src_greenplum
tbl.src_greenplum <- function(src, from, ...) {
  tbl_sql("greenplum", src = src, from = from, ...)
}

#' @export
brief_desc.src_greenplum <- function(x) {
  info <- x$info
  host <- if (info$host == "") "localhost" else info$host
  
  paste0("greenplum ", info$serverVersion, " [", info$user, "@",
         host, ":", info$port, "/", info$dbname, "]")
}

#' @export
translate_env.src_greenplum <- function(x) {
  sql_variant(
    base_scalar,
    sql_translator(.parent = base_agg,
      n = function() sql("count(*)"),
      cor = sql_prefix("corr"),
      cov = sql_prefix("covar_samp"),
      sd =  sql_prefix("stddev_samp"),
      var = sql_prefix("var_samp"),
      all = sql_prefix("bool_and"),
      any = sql_prefix("bool_or"),
      paste = function(x, collapse) build_sql("string_agg(", x, collapse, ")")
    ),
    sql_translator(.parent=base_win,
      mean  = win_recycled_greenplum("avg"),
      sum   = win_recycled_greenplum("sum"),
      min   = win_recycled_greenplum("min"),
      max   = win_recycled_greenplum("max"),
      n = function() over_greenplum(sql("COUNT(*)"), partition_group(), frame = NULL)     
    )
  )
}
