#' Data manipulation for postgre tbls.
#'
#' This page documents the specific of data manipulation for 
#' \code{\link{tbl_postgresq}} objects. See \code{\link{manip}} for the 
#' documentation of generics functions, and \code{\link{manip_sql}} for
#' general SQL documentation.
#'
#' @name manip_postgres
#' @examples
#' batting <- tbl(src_postgres("lahman"), "Batting")
#' select(batting, yearID:AB)
#' 
#' # Compute career year, rank of at bats, and cumulative at bats
#' players <- group_by(batting, playerID)
#' progress <- mutate(players, cyear = yearID - min(yearID) + 1, 
#'  rank(desc(AB)), cumsum(AB, yearID))
#' progress
#' 
#' # Progressively summarise data
#' per_year <- group_by(batting, playerID, yearID)
#' stints <- summarise(per_year, stints = max(stint))
#' filter(stints, stints > 3)
#' summarise(stints, max(stints))
#' mutate(stints, cumsum(stints, yearID))
#' 
#' # Subset grouped data
#' players <- group_by(batting, playerID)
#' best_year <- filter(players, AB == max(AB) || G == max(G))
#' best_year
NULL

#' A sql variant for translating windowed PostgreSQL functions.
#' 
#' @param variables to group by (goes into \code{PARTITION BY})
#' @export
#' @examples
#' by_team <- sql_windowed(list(quote(TeamID)))
#' # Show all functions:
#' ls(by_team)
#' 
#' translate_sql(variant = by_team,
#'    n(), 
#'    mean(AB),
#'    cumsum(AB, YearID),
#'    order(),
#'    rank(YearID)
#' )
translate_window_env.tbl_postgres <- function(x) {
  by <- translate_sql_q(groups(x))
  
  windowed_sql <- function(f, x, order) {
    build_sql(sql(f), "(", x, ") OVER ",
      "(PARTITION BY ", by, 
      if (!is.null(order)) build_sql(" ORDER BY ", order),
      ")"
    )
  }    
  
  nullary_win <- function(f) {
    function(order = NULL) windowed_sql(f, NULL, order)
  }
  unary_agg <- function(f) {
    function(x) windowed_sql(f, x, NULL)
  }
  unary_win <- function(f) {
    function(x, order = NULL) windowed_sql(f, x, order)
  }
  
  sql_variant(.parent = translate_env.src_postgres(),
    
    mean = unary_agg("AVG"),
    sum = unary_agg("SUM"),
    min = unary_agg("MIN"),
    max = unary_agg("MAX"),
    
    n = function() build_sql("COUNT(*) OVER (PARTITION BY ", by, ")"),
    
    cummean = unary_win("AVG"),
    cumsum = unary_win("SUM"),
    cummin = unary_win("MIN"),
    cummax = unary_win("MAX"),
    
    order = nullary_win("ROW_NUMBER"), 
    rank = nullary_win("RANK"),
    lag = nullary_win("LAG"),
    lead = nullary_win("LEAD")
  )
}


# players <- group_by(batting, teamID)
# translate_window_where(quote(1), players)
# translate_window_where(quote(x), players)
# translate_window_where(quote(x == 1), players)
# translate_window_where(quote(x == 1 && y == 2), players)
# translate_window_where(quote(n() > 10), players)
# translate_window_where(quote(rank() > cumsum(AB)), players)
# translate_window_where(list(quote(x == 1), quote(n() > 2)), players)

translate_window_where <- function(expr, tbl, variant = translate_window_env(tbl)) {
  
  # Simplest base case: atomic vector or name ---------------------------------
  if (is.atomic(expr) || is.name(expr)) {
    return(list(
      expr = expr,
      comp = list()
    ))
  }
    
  
  # Other base case is an aggregation function --------------------------------
  agg_f <- c("mean", "sum", "min", "max", "n", "cummean", "cummax", "cummin",
    "cumsum", "order", "rank", "lag", "lead")
  if (is.call(expr) && as.character(expr[[1]]) %in% agg_f) {
    # For now, hard code aggregation functions 
    name <- unique_name()
    
    env <- sql_env(expr, variant)
    sql <- eval(expr, env = env)
    
    return(list(
      expr = as.name(name),
      comp = setNames(list(sql), name)
    ))
  }

  # Recursive cases: list and all other functions -----------------------------
  
  if (is.list(expr)) {
    args <- lapply(expr, translate_window_where, 
      tbl = tbl, variant = variant)
    
    env <- sql_env(call, variant)
    sql <- lapply(lapply(args, "[[", "expr"), eval, env = env)    
  } else {
    args <- lapply(expr[-1], translate_window_where, 
      tbl = tbl, variant = variant)
    
    call <- as.call(c(expr[[1]], lapply(args, "[[", "expr")))
    env <- sql_env(call, variant)
    sql <- eval(call, env = env)
  }
  
  comps <- unlist(lapply(args, "[[", "comp"), recursive = FALSE)

  list(
    expr = sql, 
    comp = comps
  )
}

unique_name <- local({
  i <- 0
  
  function() {
    i <<- i + 1
    paste0("_W", i)
  }
})
