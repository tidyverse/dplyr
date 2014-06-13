#' Connect to Oracle.
#'
#' Use \code{src_oracle} to connect to an existing Oracle database,
#' and \code{tbl} to connect to tables within that database.
#'
#' @template db-info
#' @param dbname Database name
#' @param user,password User name and password (if needed)
#' @param ... for the src, other arguments passed on to the underlying
#'   database connector, \code{dbConnect}. For the tbl, included for
#'   compatibility with the generic, but otherwise ignored.
#' @param src an Oracle src created with \code{src_oracle}.
#' @param from Either a string giving the name of table in database, or
#'   \code{\link{sql}} described a derived table or compound join.
#' @export
#' @examples
#' \dontrun{
#' # Connection basics ---------------------------------------------------------
#' # To connect to a database first create a src:
#' my_db <- src_oracle(host = "TNShost", user = "hadley",
#'   password = "pass")
#' # Then reference a tbl within that src
#' my_tbl <- tbl(my_db, "my_table")
#' }
#'
#' # Here we'll use the Lahman database: to create your own local copy,
#' # create a local database called "lahman", or tell lahman_oracle() how to
#' # a database that you can write to
#'
#' if (has_lahman("oracle")) {
#' # Methods -------------------------------------------------------------------
#' batting <- tbl(lahman_oracle(), "Batting")
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
#' player_info <- select(tbl(lahman_oracle(), "Master"), playerID, hofID,
#'   birthYear)
#' hof <- select(filter(tbl(lahman_oracle(), "HallOfFame"), inducted == "Y"),
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
#' batting2008 <- tbl(lahman_oracle(),
#'   sql('SELECT * FROM "Batting" WHERE "yearID" = 2008'))
#' batting2008
#' }
src_oracle <- function(dbname = NULL, user = NULL,
                         password = NULL, ...) {
  if (!require("ROracle")) {
    stop("ROracle package required to connect to Oracle db", call. = FALSE)
  }
  

  user <- user %||% if (in_travis()) "oracle" else ""
  
  con  <- dbi_connect(Oracle(), username = user %||% "", password = password %||% "", dbname = dbname %||% "", ...)

  info <- db_info(con)
  
  src_sql("oracle", con,
          info = info, disco = db_disconnector(con, "oracle"))
}

#' @export
#' @rdname src_oracle
tbl.src_oracle <- function(src, from, ...) {
  tbl_sql("oracle", src = src, from = from, ...)
}

#' @export
brief_desc.src_oracle <- function(x) {  

   info <- x$info
   paste0("oracle ", info$serverVersion, " [", info$username, "@",
          info$dbname,"]")
}

#' @export
translate_env.src_oracle <- function(x) {
  sql_variant(
    base_scalar,
    sql_translator(.parent = base_agg,
     n = function() sql("count(*)"),
     cor = sql_prefix("corr"),
     cov = sql_prefix("covar_samp"),
     sd =  sql_prefix("stddev"),
     var = sql_prefix("var_samp"),
     #all = sql_prefix("bool_and"),
     #any = sql_prefix("bool_or"),
     paste = function(x, collapse) build_sql("string_agg(", x, collapse, ")")                  

    ),
    base_win
  )
}
