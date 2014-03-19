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

win_rank_greenplum <- function(f) {
  force(f)
  function(order = NULL) {
    over_greenplum(build_sql(sql(f), list()), partition_group(), order %||% partition_order())
  }
}
win_recycled_greenplum <- function(f) {
  force(f)
  function(x) {
    over_greenplum(build_sql(sql(f), list(x)), partition_group(), NULL, frame = NULL)
  }
}
win_cumulative_greenplum <- function(f) {
  force(f)
  function(x) {
    over_greenplum(build_sql(sql(f), list(x)), partition_group(), partition_order(), frame = c(-Inf, 0))
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
    sql_translator(.parent = base_win,
      # rank functions have a single order argument that overrides the default
      row_number   = win_rank_greenplum("row_number"),
      min_rank     = win_rank_greenplum("rank"),
      rank         = win_rank_greenplum("rank"),
      dense_rank   = win_rank_greenplum("dense_rank"),
      percent_rank = win_rank_greenplum("percent_rank"),
      cume_dist    = win_rank_greenplum("cume_dist"),
      ntile        = function(order_by, n) {
        over_greenplum(
          build_sql("NTILE", list(n)),partition_group(),order_by %||% partition_order())
        },
                   
      # Recycled aggregate fuctions take single argument, don't need order and
      # include entire partition in frame.
      mean  = win_recycled_greenplum("avg"),
      sum   = win_recycled_greenplum("sum"),
      min   = win_recycled_greenplum("min"),
      max   = win_recycled_greenplum("max"),
      n     = function() {
        over_greenplum(sql("COUNT(*)"), partition_group(), frame = c(-Inf, Inf))
        },
                   
      # Cumulative function are like recycled aggregates except that R names
      # have cum prefix, order_by is inherited and frame goes from -Inf to 0.
      cummean = win_cumulative_greenplum("mean"),
      cumsum  = win_cumulative_greenplum("sum"),
      cummin  = win_cumulative_greenplum("min"),
      cummax  = win_cumulative_greenplum("max"),
                   
      # Finally there are a few miscellaenous functions that don't follow any
      # particular pattern
      nth = function(x, order = NULL) {
        over_greenplum(build_sql("NTH_VALUE", list(x)), partition_group(), order %||% partition$order())
        },
      first = function(x, order = NULL) {
        over_greenplum(sql("FIRST_VALUE()"), partition_group(), order %||% partition_order())
        },
      last = function(x, order = NULL) {
        over_greenplum(sql("LAST_VALUE()"), partition_group(), order %||% partition_order())
        },
                   
      lead = function(x, n = 1L, default = NA, order = NULL) {
        over_greenplum(
          build_sql("LEAD", list(x, n, default)),
          partition_group(),
          order %||% partition_order()
          )
        },
      lag = function(x, n = 1L, default = NA, order = NULL) {
        over_greenplum(
          build_sql("LAG", list(x, n, default)),
          partition_group(),
          order %||% partition_order()
          )
        },
                   
      order_by = function(order_by, expr) {
        over_greenplum(expr, partition_group(), order_by)
      }                
    )
  )
}
