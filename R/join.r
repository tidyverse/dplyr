#' Join two tbls together
#' 
#' @param x,y tbls to join
#' @param by a character vector of variables to join by
#' @param type a string giving the join type. Possible values are left (the 
#'   default), right, full and inner. Not all types will be supported by all
#'   tbls.
#' @param copy If \code{x} and \code{y} are not from the same data source,
#'   and \code{copy} is \code{TRUE}, then \code{y} will be copied into the 
#'   same src as \code{x}.  This allows you to join tables across srcs, but
#'   it is a potentially expensive operation so you must opt into it.
#' @param ... other parameters passed onto individual methods
#' @export
join <- function(x, y, by = NULL, type = "left", copy = FALSE, ...) {
  UseMethod("join")
}

#' Join SQLite tbls.
#' 
#' @inheritParams join
#' @param copy If \code{x} and \code{y} are not from the same data source,
#'   and \code{copy} is \code{TRUE}, then \code{y} will be copied into a 
#'   temporary table in same database as \code{x}. \code{join} will automatically 
#'   run \code{ANALYSE} on the created table in the hope that this will make
#'   you queries as efficient as possible by giving more data to the query
#'   planner.
#'   
#'   This allows you to join tables across srcs, but it's potentially expensive 
#'   operation so you must opt into it.
#' @param auto_index if \code{copy} is \code{TRUE}, automatically create 
#'   indices for the variables in \code{by}. This may speed up the join if
#'   there are matching indices in \code{x}.
#' @method join tbl_sqlite
#' @export
#' @examples
#' l <- lahman()
#' batting <- tbl(l, "Batting")
#' 
#' grid <- expand.grid(
#'   teamID = c("WAS", "ATL", "PHI", "NYA"), 
#'   yearID = 2010:2012)
#' top4a <- join(batting, grid, copy = TRUE, type = "inner")
#' system.time(as.data.frame(top4a))
#' 
#' # Indices don't really help here because there's no matching index on
#' # batting
#' top4b <- join(batting, grid, copy = TRUE, type = "inner", auto_index = TRUE)
#' system.time(as.data.frame(top4b))
join.tbl_sqlite <- function(x, y, by = NULL, type = "left", copy = FALSE, 
                            auto_index = FALSE, ...) {
  type <- match.arg(type, c("left", "right", "inner", "full"))
  
  if (is.null(by)) {
    by <- intersect(tbl_vars(x), tbl_vars(y))
    message("Joining by: ", capture.output(dput(by)))
  }

  # If y is a character, assume it's the name of a table
  if (is.character(y)) {
    y <- tbl(x$src, y)
  }
  
  # If y is not a tbl_sqlite, create it  
  if (!inherits(y, "tbl_sqlite")) {
    if (!copy) {
      stop("y is not a tbl_sqlite, and copy is FALSE", call. = FALSE)
    }
    
    temp_table <- ident(random_table_name())
    y <- write_table(x$src, temp_table, y)
    
    if (auto_index) {
      create_index(y, by)
    }
    run_sql(x$src$con, build_sql("ANALYZE ", temp_table))
  }
    
  if (!same_src(x$src, y$src)) {
    stop("x and y must use same database connection", call. = FALSE)    
  }
  
  join <- switch(type, left = sql("LEFT"), inner = sql("INNER"),
    right = stop("Right join not supported", call. = FALSE),
    full = stop("Full join not supported", call. = FALSE))
  
  from_x <- select_qry(x)$sql
  from_y <- select_qry(y)$sql
  
  from <- build_sql(list(from_x), "\n\n", 
    join, " JOIN \n\n" , 
    list(from_y), "\n\n",
    "USING ", lapply(by, ident))
  
  tbl_sql(c("sqlite_tbl", "sqlite"), src = x$src, table = from)
}

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}