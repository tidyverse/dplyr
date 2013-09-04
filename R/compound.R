#' Create a compound select.
#' 
#' Combine together the results of multiple SQL queries,
#' 
#' @param ... \code{tbl}s to combine
#' @param type type of combination.
#' @example
#' l <- lahman()
#' batting <- select(tbl(l, "Batting"), playerID:lgID, G_batting)
#' pitching <- select(filter(tbl(l, "Fielding"), POS == "P"), playerID:lgID, GS)
#' outfield <- select(filter(tbl(l, "Fielding"), POS == "OF"), playerID:lgID, GS)
#'
#' plays <- compound(batting, pitching, outfield)
#' head(plays)
compound <- function(..., type) {
  UseMethod("compound")
}

compound.tbl_sqlite <- function(..., type = "union all") {
  type <- match.arg(tolower(type), 
    c("union all", "union", "intersect", "exclude"))
 
  tbls <- list(...)
  src <- tbls[[1]]$src
  for (tbl in tbls) {
    if (!is.null(tbl$arrange)) {
      stop("tbls used in a compound select can not be ordered", call. = FALSE)
    }
    
    if (!identical(src$con, tbl$src$con)) {
      stop("All tbls must come from the same source", call. = FALSE)
    }
  }
  
  from <- do.call("c", lapply(tbls, from))
  compounder <- paste0(" ", toupper(type), " ")
  selects <- escape(from, collapse = compounder, parens = FALSE)
  
  tbl_sql(c("sqlite_compound", "sqlite"), src = src, select = selects)
}

select.tbl_sqlite_compound <- function(.data, ...) {
  stop("Can not select columns in compound query", .call = FALSE)
}
mutate.tbl_sqlite_compound <- function(.data, ...) {
  stop("Can not mutate columns in compound query", .call = FALSE)
}
summarise.tbl_sqlite_compound <- function(.data, ...) {
  stop("Can not summarise columns in compound query", .call = FALSE)
}

sql_compound_select <- function(x, order_by = NULL, limit = NULL, offset = NULL,
                                n = -1L, 
                                explain = getOption("dplyr.explain_sql"),
                                show = getOption("dplyr.show_sql"),
                                into_table = NULL) {  
  order_by <- order_by %||% trans_sqlite(x$arrange)
  
  sql <- build_sql(x$select, 
    if (!is.null(order_by)) build_sql("ORDER_BY ", order_by),
    if (!is.null(limit)) build_sql("LIMIT ", limit),
    if (!is.null(offset)) build_sql("OFFSET ", offset)
  )
  
  if (is.null(into_table)) {
    exec_sql(x$src$con, sql, n = n, explain = explain, show = show)  
  } else {
    sql <- build_sql("CREATE TEMPORARY TABLE ", ident(into_table), " AS ", sql)
    exec_sql(x$src$con, sql, explain = explain, show = show, fetch = FALSE)  
  } 
}

