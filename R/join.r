join <- function(x, y, by = NULL, type = "left", ...) {
  UseMethod("join")
}

join.tbl_sqlite <- function(x, y, by = NULL, type = "left", upload = FALSE, ...) {
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
    if (!upload) {
      stop("y is not a tbl_sqlite, and upload is FALSE", call. = FALSE)
    }
    
    y <- write_table(x$src, random_table_name(), y)
  }
    
  if (!identical(x$src$con, y$src$con)) {
    stop("x and y must use same database connection", call. = FALSE)    
  }
  
  join <- switch(type, left = sql("LEFT"), inner = sql("INNER"),
    right = stop("Right join not supported", call. = FALSE),
    full = stop("Full join not supported", call. = FALSE))
  
  from <- build_sql(from(x), "\n\n", 
    join, " JOIN \n\n" , 
    from(y), "\n\n",
    "USING ", lapply(by, ident))
  
  tbl_sql(c("sqlite_tbl", "sqlite"), src = x$src, table = from)
}

from <- function(x) {
  if (is.null(x$filter) && is.null(x$arrange) && is.null(x$select)) {
    return(ident(x$table))
  }
  
  build_sql(list(select_query(
    from = x$table,
    select = x$select %||% "*",
    where = trans_sqlite(x$filter),
    order_by = trans_sqlite(x$arrange))))
}

random_table_name <- function(n = 10) {
  paste0(sample(letters, n, replace = TRUE), collapse = "")
}