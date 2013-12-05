#' Create an SQL tbl (abstract)
#'
#' This method shouldn't be called be users - it should only be used by
#' backend implementors who are creating backends that extend the basic
#' sql behaviour.
#'
#' @keywords internal
#' @export
#' @param subclass name of subclass
#' @param ... needed for agreement with generic. Not otherwise used.
#' @param vars If known, the names of the variables in the tbl. This is 
#'   relatively expensive to determine automatically, so is cached throughout
#'   dplyr. However, you should usually be able to leave this blank and it
#'   will be determined from the context.
tbl_sql <- function(subclass, src, from, ..., vars = NULL) {
  assert_that(is.character(from), length(from) == 1)

  
  if (!is.sql(from)) { # Must be a character string
    if (isFALSE(db_has_table(src$con, from))) {
      stop("Table ", from, " not found in database ", src$path, call. = FALSE)
    }
    
    from <- ident(from)
  } else if (!is.join(from)) { # Must be arbitrary sql
    # Abitrary sql needs to be wrapped into a named subquery
    from <- build_sql("(", from, ") AS ", ident(unique_name()), con = src$con)
  }
  
  tbl <- make_tbl(c(subclass, "sql"),
    src = src,              # src object
    from = from,            # table, join, or raw sql
    select = vars,          # SELECT: list of symbols
    summarise = FALSE,      #   interpret select as aggreagte functions?
    mutate = FALSE,         #   do select vars include new variables?
    where = NULL,           # WHERE: list of calls
    group_by = NULL,        # GROUP_BY: list of names
    order_by = NULL         # ORDER_BY: list of calls
  )
  update(tbl)
}

#' @export
update.tbl_sql <- function(object, ...) {
  args <- list(...)
  assert_that(only_has_names(args, c("select", "where", "group_by", "order_by")))
  
  for (nm in names(args)) {
    object[[nm]] <- args[[nm]]
  }
  
  # Figure out variables
  if (is.null(object$select)) {
    if (is.ident(object$from)) {
      var_names <- table_fields(object$src$con, object$from)
    } else {
      var_names <- qry_fields(object$src$con, object$from)
    }
    vars <- lapply(var_names, as.name)
    object$select <- vars
  }
  
  object$query <- build_query(object)  
  object
} 

#' @export
same_src.tbl_sql <- function(x, y) {
  if (!inherits(y, "tbl_sql")) return(FALSE)
  same_src(x$src, y$src)
}

#' @export
tbl_vars.tbl_sql <- function(x) {
  x$query$vars()
}

#' @export
groups.tbl_sql <- function(x) {
  x$group_by
}

# Grouping methods -------------------------------------------------------------

#' @export
ungroup.tbl_sql <- function(x, ...) {
  update(x, group_by = NULL)
}

#' @export
group_size.tbl_sql <- function(x) {
  df <- collect(summarise(x, n = n()))
  df$n
}

# Standard data frame methods --------------------------------------------------

#' @export
as.data.frame.tbl_sql <- function(x, row.names = NULL, optional = NULL,
  ..., n = 1e5L) {
  x$query$fetch(n)
}

#' @export
print.tbl_sql <- function(x, ...) {
  cat("Source: ", brief_desc(x$src), "\n", sep = "")
  
  if (inherits(x$from, "ident")) {
    cat(wrap("From: ", x$from, " ", dim_desc(x)))
  } else {
    cat(wrap("From: <derived table> ", dim_desc(x)))    
  }
  cat("\n")
  if (!is.null(x$where)) {
    cat(wrap("Filter: ", commas(x$where)), "\n")
  }
  if (!is.null(x$order_by)) {
    cat(wrap("Arrange: ", commas(x$order_by)), "\n")
  }
  if (!is.null(x$group_by)) {
    cat(wrap("Grouped by: ", commas(x$group_by)), "\n")
  }
  
  cat("\n")
  
  trunc_mat(x)
}

brief_desc <- function(x) UseMethod("brief_desc")

#' @export
dimnames.tbl_sql <- function(x) {
  list(NULL, tbl_vars.tbl_sql(x))
}

#' @export
dim.tbl_sql <- function(x) {
  if (!inherits(x$from, "ident")) {
    n <- NA
  } else {
    n <- x$query$nrow()
  }
  
  p <- x$query$ncol()
  c(n, p)
}

#' @export
head.tbl_sql <- function(x, n = 6L, ...) {
  assert_that(length(n) == 1, n > 0L)
  
  build_query(x, limit = n)$fetch()
}

#' @export
tail.tbl_sql <- function(x, n = 6L, ...) {
  stop("tail is not supported by sql sources", call. = FALSE)
}

# SQL select generation --------------------------------------------------------

build_query <- function(x, limit = NULL) {  
  assert_that(is.null(limit) || (is.numeric(limit) && length(limit) == 1))  
  translate <- function(expr, ...) {
    translate_sql_q(expr, tbl = x, env = NULL, ...)
  }
  
  if (x$summarise) {
    # Summarising, so SELECT needs to contain grouping variables
    select <- c(x$group_by, x$select)
    select <- select[!duplicated(select)]
    
    select_sql <- translate(select)
    vars <- auto_names(select)
    
    group_by_sql <- translate(x$group_by)
    order_by_sql <- translate(x$order_by)
  } else {
    # Not in summarise, so assume functions are window functions
    select_sql <- translate(x$select, window = uses_window_fun(x$select, x))
    vars <- auto_names(x$select)

    # Don't use group_by - grouping affects window functions only
    group_by_sql <- NULL    
    
    # If the user requested ordering, ensuring group_by is included
    # Otherwise don't, because that may make queries substantially slower
    if (!is.null(x$order_by) && !is.null(x$group_by)) {
      order_by_sql <- translate(c(x$group_by, x$order_by))
    } else {
      order_by_sql <- translate(x$order_by)
    }
  }
  
  if (!uses_window_fun(x$where, x)) {
    from_sql <- x$from
    where_sql <- translate(x$where)
  } else {
    # window functions in WHERE need to be performed in subquery
    where <- translate_window_where(x$where, x, con = x$src$con)
    base_query <- update(x, 
      group_by = NULL,
      where = NULL,
      select = c(x$select, where$comp))$query
    
    from_sql <- build_sql("(", base_query$sql, ") AS ", ident(unique_name()), 
      con = x$src$con)
    where_sql <- translate(where$expr)
  }
  
  
  sql <- sql_select(x$src$con, from = from_sql, select = select_sql, 
    where = where_sql, order_by = order_by_sql, group_by = group_by_sql, 
    limit = limit)
  query(x$src$con, sql, vars)
}

uses_window_fun <- function(x, tbl) {
  if (is.null(x)) return(FALSE)
  if (is.list(x)) {
    calls <- unlist(lapply(x, all_calls))
  } else {
    calls <- all_calls(x)
  }

  win_f <- ls(envir = translate_env(tbl)$window)
  any(calls %in% win_f)
}
