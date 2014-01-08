#' A bigquery data source.
#'
#' Use \code{src_bigquery} to connect to an existing bigquery dataset,
#' and \code{tbl} to connect to tables within that database. 
#' 
#' @template db-info
#' @param project project id or name
#' @param dataset dataset name
#' @param billing billing project, if different to \code{project}
#' @param from Either a string giving the name of table in database, or
#'   \code{\link{sql}} described a derived table or compound join.
#' @param ... Included for compatibility with the generic, but otherwise 
#'   ignored.
#' @param src a bigquery src created with \code{src_bigquery}.
#' @export
#' @examples
#' # Connection basics ---------------------------------------------------------
#' \dontrun{
#' # To connect to a database first create a src:
#' my_db <- src_bigquery("myproject", "mydataset")
#' # Then reference a tbl within that src
#' my_tbl <- tbl(my_db, "my_table")
#' }
#'
#' # Here we'll use the Lahman database: to create your own local copy,
#' # create a local database called "lahman", or tell lahman_bigqueryql() how to 
#' # a database that you can write to
#' 
#' if (has_lahman("bigquery") && interactive()) {
#' # Methods -------------------------------------------------------------------
#' batting <- tbl(lahman_bigquery(), "Batting")
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
#' filter(players, AB == max(AB) | G == max(G))
#' # Not supported yet:
#' \dontrun{
#' mutate(players, cyear = yearID - min(yearID) + 1, 
#'  cumsum(AB, yearID))
#' }
#' mutate(players, rank())
#'  
#' # When you group by multiple level, each summarise peels off one level
#' per_year <- group_by(batting, playerID, yearID)
#' stints <- summarise(per_year, stints = max(stint))
#' filter(stints, stints > 3)
#' summarise(stints, max(stints))
#' # Not supported yet:
#' \dontrun{mutate(stints, cumsum(stints, yearID))}
#' # But other window functions are:
#' mutate(players, rank = rank(ab))
#'
#' # Joins ---------------------------------------------------------------------
#' player_info <- select(tbl(lahman_bigquery(), "Master"), playerID, hofID, 
#'   birthYear)
#' hof <- select(filter(tbl(lahman_bigquery(), "HallOfFame"), inducted == "Y"),
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
#' batting2008 <- tbl(lahman_bigqueryql(),
#'   sql("SELECT * FROM Batting WHERE YearID = 2008"))
#' batting2008
#' }
src_bigquery <- function(project, dataset, billing = project) {
  assert_that(is.string(project), is.string(dataset), is.string(billing))
  
  if (!require("bigrquery")) {
    stop("bigrquery package required to connect to bigquery db", call. = FALSE)
  }

  con <- structure(list(project = project, dataset = dataset,
    billing = billing), class = "bigquery")
  src_sql("bigquery", con)
}

#' @export
#' @rdname src_bigquery
tbl.src_bigquery <- function(src, from, ...) {
  tbl_sql("bigquery", src = src, from = from, ...)
}

#' @export
brief_desc.src_bigquery <- function(x) {
  paste0("bigquery [", x$con$project, "/", x$con$dataset, "]")
}

#' @export
translate_env.src_bigquery <- function(x) {
  sql_variant(
    sql_translator(.parent = base_scalar,
      # Casting
      as.logical = sql_prefix("boolean"),
      as.numeric = sql_prefix("float"),
      as.double = sql_prefix("float"),
      as.integer = sql_prefix("integer"),
      as.character = sql_prefix("string"),
      
      # Date/time
      Sys.date = sql_prefix("current_date"),
      Sys.time = sql_prefix("current_time"),
      
      # Regular expressions
      grepl = function(match, x) {
        sprintf("REGEXP_MATCH(%s, %s)", escape(x), escape(match))
      },
      gsub = function(match, replace, x) {
        sprintf("REGEXP_REPLACE(%s, %s, %s)", escape(x), escape(match),
          escape(replace))
      },
      
      # stringr equivalents
      str_detect = function(x, match) {
        sprintf("REGEXP_MATCH(%s, %s)", escape(x), escape(match))
      },
      str_extract = function(x, match) {
        sprintf("REGEXP_EXTRACT(%s, %s)", escape(x), escape(match))
      },
      str_replace = function(x, match, replace) {
        sprintf("REGEXP_REPLACE(%s, %s, %s)", escape(x), escape(match),
          escape(replace))
      }
    ),
    sql_translator(.parent = base_agg,
      n = function() sql("count(*)"),
      "%||%" = sql_prefix("concat"),
      sd = sql_prefix("stddev")
    ),
    base_win
  )
}

globalVariables(c("insert_upload_job", "wait_for", "list_tables", 
  "insert_upload_job", "query_exec", "get_table"))
