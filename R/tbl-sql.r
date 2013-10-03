#' Create an SQL tbl (abstract)
#'
#' This method shouldn't be called be users - it should only be used by
#' backend implementors who are creating backends that extend the basic
#' sql behaviour.
#'
#' @keywords internal
#' @export
#' @param subclass name of subclass
#' @param ... other fields needed by the subclass
#' @param select,filter,arrange default sql components.
tbl_sql <- function(subclass, ..., select = NULL, filter = NULL,
                       arrange = NULL) {
  make_tbl(c(subclass, "sql"), ..., 
    select = select, 
    filter = filter,
    arrange = arrange)
}

is_table <- function(x) {
  if (!inherits(x$from, "ident")) return(FALSE)
  
  identical(x$select, list(star())) && is.null(x$where) && is.null(x$group_by) && 
    is.null(x$order_by)
}

#' @S3method update tbl_sql
update.tbl_sql <- function(object, ..., summarise = FALSE) {
  args <- list(...)
  assert_that(only_has_names(args, c("select", "where", "group_by", "order_by")))
  
  for (nm in names(args)) {
    object[[nm]] <- args[[nm]]
  }
  object$summarise <- summarise
  
  object$query <- build_query(object)
  
  object
} 

#' @S3method same_src tbl_sql
same_src.tbl_sql <- function(x, y) {
  if (!inherits(y, "tbl_sql")) return(FALSE)
  same_src(x$src, y$src)
}

#' @S3method tbl_vars tbl_sql
tbl_vars.tbl_sql <- function(x) {
  x$query$vars()
}

#' @S3method groups tbl_sql
groups.tbl_sql <- function(x) {
  x$group_by
}

# Grouping methods -------------------------------------------------------------

#' @S3method group_by tbl_sql
group_by.tbl_sql <- function(x, ...) {
  input <- dots(...)
  if (!all_apply(input, is.name)) {
    stop("May only group by variable names, not expressions", call. = FALSE)
  }
  
  update(x, group_by = c(x$group_by, input))
}

#' @S3method ungroup tbl_sql
ungroup.tbl_sql <- function(x, ...) {
  update(x, group_by = NULL)
}

#' @S3method group_size tbl_sql
group_size.tbl_sql <- function(x) {
  df <- collect(summarise(x, n()))
  df[[length(df)]]
}

# Standard data frame methods --------------------------------------------------

#' @S3method as.data.frame tbl_sql
as.data.frame.tbl_sql <- function(x, row.names = NULL, optional = NULL,
  ..., n = 1e5L) {
  x$query$fetch(n)
}

#' @S3method print tbl_sql
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

#' @S3method dimnames tbl_sql
dimnames.tbl_sql <- function(x) {
  list(NULL, tbl_vars.tbl_sql(x))
}

#' @S3method dim tbl_sql
dim.tbl_sql <- function(x) {
  if (!inherits(x$from, "ident")) {
    n <- NA
  } else {
    n <- x$query$nrow()
  }
  
  p <- x$query$ncol()
  c(n, p)
}

#' @S3method head tbl_sql
head.tbl_sql <- function(x, n = 6L, ...) {
  assert_that(length(n) == 1, n > 0L)
  
  build_query(x, limit = n)$fetch()
}

#' @S3method tail tbl_sql
tail.tbl_sql <- function(x, n = 6L, ...) {
  stop("tail is not supported by sql sources", call. = FALSE)
}

# SQL select generation --------------------------------------------------------

build_query <- function(x, select = x$select, from = x$from, 
                                    where = x$where, group_by = x$group_by, 
                                    having = NULL, order_by = x$order_by, 
                                    limit = NULL, offset = NULL) {  
  assert_that(
    is.lang.list(select), 
    is.sql(from),
    is.lang.list(where), 
    is.lang.list(group_by),
    is.lang.list(having), 
    is.lang.list(order_by), 
    is.null(limit) || (is.numeric(limit) && length(limit) == 1), 
    is.null(offset) || (is.lang(offset) && length(offset) == 1))

  # Make sure select contains grouping variables
  if (!has_star(select) && !is.null(group_by)) {
    # Can't use unique because it strips names
    select <- c(group_by, select)
    select <- select[!duplicated(select)]
  }
  
  # Translate expression to SQL strings ------------
  
  translate <- function(expr) {
    translate_sql_q(expr, source = x$src, env = NULL)
  }
  
  # If doing grouped subset, first make subquery, then evaluate where
  if (!is.null(group_by) && !is.null(where)) {
    # any aggregate or windowing function needs to be extract
    where2 <- translate_window_where(where, x)
    
    base_query <- update(x, 
      group_by = NULL,
      where = NULL,
      select = c(select, where2$comp))$query
    from <- build_sql("(", base_query$sql, ") AS ", ident(unique_name()))
    
    select <- expand_star(select, x)
    where <- where2$expr    
  }
  
  # group by has different behaviour depending on whether or not
  # we're summarising
  if (x$summarise) {
    select <- translate(select)

    group_by <- translate(group_by)
    order_by <- translate(order_by)
  } else {
    if (!is.null(group_by)) {
      select <- translate_select(select, x)      
      order_by <- translate(c(group_by, order_by))
    } else {
      select <- translate(select)
      order_by <- translate(order_by)
    }
    group_by <- NULL    
  }
  
  where <- translate(where)
  having <- translate(having)
  
  sql <- sql_select(x$src, from = from, select = select, where = where, 
    order_by = order_by, group_by = group_by, limit = limit, offset = offset)
  query(x$src$con, sql)
}
