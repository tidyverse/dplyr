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
  make_tbl(c(subclass, "sql"), ..., select = select, filter = filter,
    arrange = arrange)
}

is_table <- function(x) {
  if (!inherits(x$from, "ident")) return(FALSE)
  
  identical(x$select, list(star())) && is.null(x$where) && is.null(x$group_by) && 
    is.null(x$order_by)
}

#' @S3method update tbl_sql
update.tbl_sql <- function(object, ...) {
  args <- list(...)
  assert_that(only_has_names(args, c("select", "where", "group_by", "order_by")))
  
  for (nm in names(args)) {
    object[[nm]] <- args[[nm]]
  }
  object$query <- qry_select(object)
  
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
  
  update(x, 
    group_by = c(x$group_by, input), 
    select = x$select)
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
  x$query$fetch_df(n)
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
  
  rows <- nrow(x)
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
  
  qry_select(x, limit = n)$fetch_df()
}

#' @S3method tail tbl_sql
tail.tbl_sql <- function(x, n = 6L, ...) {
  stop("tail is not supported by sql sources", call. = FALSE)
}
