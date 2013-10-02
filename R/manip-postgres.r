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
#' players <- group_by(batting, playerID)
#' mutate(players, cyear = yearID - min(yearID) + 1, 
#'  rank(desc(AB)), cumsum(AB, yearID))
#' 
#' mutate(players, rank(AB), cumsum(AB, yearID))
NULL

#' @rdname manip_postgres
#' @export
#' @method summarise tbl_postgres
summarise.tbl_postgres <- function(.data, ...) {
  input <- partial_eval(dots(...), .data, parent.frame())
  
  if (!identcal(.data$select, star())) {
    stop("Can not summarise tbl_postgres that has been ", "
      select()ed or mutate()d. collapse() it first.", call. = FALSE)
  }
  
  .data$summarise <- TRUE
  .data <- update(.data, select = input)
  
  update(
    collapse(.data),
    group_by = drop_last(.data$group_by)
  )
}


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



#' @S3method qry_select tbl_postgres
qry_select.tbl_postgres <- function(x, select = x$select, from = x$from, 
                                      where = x$where, group_by = x$group_by, 
                                      having = NULL, order_by = x$order_by, 
                                      limit = NULL, offset = NULL) {  
  if (!has_star(select)) {
    # Can't use unique because it strips names
    select <- c(group_by, select)
    select <- select[!duplicated(select)]
  }
  translate <- function(expr) {
    translate_sql_q(expr, source = x$src, env = NULL)
  }
  select <- translate_select(select, x)
  where <- translate(where)

  # only summarise + group needs group by
  group_by <- if (isTRUE(x$summarise)) translate(group_by) else NULL
  having <- translate(having)
  order_by <- translate(order_by)
  
  qry_select(x$src, from = from, select = select, where = where, 
    order_by = order_by, group_by = group_by, limit = limit, offset = offset)
}
