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
    if (!db_has_table(src$con, from)) {
      stop("Table ", from, " not found in database ", src$path, call. = FALSE)
    }
    
    from <- ident(from)
  } else if (!is.join(from)) { # Must be arbitrary sql
    # Abitrary sql needs to be wrapped into a named subquery
    from <- build_sql("(", from, ") AS ", ident(random_table_name()), con = src$con)
  }
  
  tbl <- make_tbl(c(subclass, "sql"),
    src = src, 
    from = from,
    summarise = FALSE, 
    new_vars = TRUE,
    select = vars,
    where = NULL,
    group_by = NULL,
    order_by = NULL
  )
  update(tbl)
}

#' @S3method update tbl_sql
update.tbl_sql <- function(object, ..., summarise = FALSE) {
  args <- list(...)
  assert_that(only_has_names(args, c("select", "where", "group_by", "order_by")))
  
  for (nm in names(args)) {
    object[[nm]] <- args[[nm]]
  }
  object$summarise <- summarise
  
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
  
  # Make sure select contains grouping variables
  if (!is.null(object$group_by)) {
    # Can't use unique because it strips names
    select <- c(object$group_by, object$select)
    object$select <- select[!duplicated(select)]
  }
  
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

#' @export
"groups<-.tbl_sql" <- function(x, value) {
  if (!all_apply(value, is.name)) {
    stop("May only group by variable names, not expressions", call. = FALSE)
  }
  
  update(x, group_by = unname(value))
}

#' @S3method ungroup tbl_sql
ungroup.tbl_sql <- function(x, ...) {
  update(x, group_by = NULL)
}

#' @S3method group_size tbl_sql
group_size.tbl_sql <- function(x) {
  df <- collect(summarise(x, n = n()))
  df$n
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
  
  # Translate expression to SQL strings ------------
  
  translate <- function(expr) {
    translate_sql_q(expr, source = x$src, env = NULL)
  }
  
  # If doing grouped subset, first make subquery, then evaluate where
  if (!is.null(group_by) && !is.null(where)) {
    # any aggregate or windowing function needs to be extract
    where2 <- translate_window_where(where, x, con = x$src$con)
    
    base_query <- update(x, 
      group_by = NULL,
      where = NULL,
      select = c(select, where2$comp))$query
    from <- build_sql("(", base_query$sql, ") AS ", ident(unique_name()), 
      con = x$src$con)
    
    where <- where2$expr    
  }
  
  # group by has different behaviour depending on whether or not
  # we're summarising
  if (x$summarise) {
    select_sql <- translate(select)

    group_by <- translate(group_by)
    order_by <- translate(order_by)
  } else {
    if (!is.null(group_by)) {
      select_sql <- translate_select(select, x)      
    } else {
      select_sql <- translate(select)
    }
    
    # If the user request ordering, ensuring group_by is included
    # Otherwise don't, because that may make queries substantially slower
    if (!is.null(order_by) && !is.null(group_by)) {
      order_by <- translate(c(group_by, order_by))
    } else {
      order_by <- translate(order_by)
    }
    
    group_by <- NULL    
  }
  
  where <- translate(where)
  having <- translate(having)
  
  sql <- sql_select(x$src$con, from = from, select = select_sql, where = where, 
    order_by = order_by, group_by = group_by, limit = limit, offset = offset)
  query(x$src$con, sql, select)
}
