

# select_query ------------------------------------------------------------

#' @export
#' @rdname sql_build
select_query <- function(from,
                         select = sql("*"),
                         where = character(),
                         group_by = character(),
                         having = character(),
                         order_by = character(),
                         limit = NULL,
                         distinct = FALSE) {

  stopifnot(is.character(select))
  stopifnot(is.character(where))
  stopifnot(is.character(group_by))
  stopifnot(is.character(having))
  stopifnot(is.character(order_by))
  stopifnot(is.null(limit) || (is.numeric(limit) && length(limit) == 1L))
  stopifnot(is.logical(distinct), length(distinct) == 1L)

  structure(
    list(
      from = from,
      select = select,
      where = where,
      group_by = group_by,
      having = having,
      order_by = order_by,
      distinct = distinct,
      limit = limit
    ),
    class = c("select_query", "query")
  )
}

# List clauses used by a query, in the order they are executed in
select_query_clauses <- function(x) {
  present <- c(
    where =    length(x$where) > 0,
    group_by = length(x$group_by) > 0,
    having =   length(x$having) > 0,
    select =   !identical(x$select, sql("*")),
    distinct = x$distinct,
    order_by = length(x$order_by) > 0,
    limit    = !is.null(x$limit)
  )

  ordered(names(present)[present], levels = names(present))
}


#' @export
print.select_query <- function(x, ...) {
  cat(
    "<SQL SELECT",
    if (x$distinct) " DISTINCT", ">\n",
    sep = ""
  )
  cat("From:     ", gsub("\n", " ", sql_render(x$from, root = FALSE)), "\n", sep = "")

  if (length(x$select))   cat("Select:   ", named_commas(x$select), "\n", sep = "")
  if (length(x$where))    cat("Where:    ", named_commas(x$where), "\n", sep = "")
  if (length(x$group_by)) cat("Group by: ", named_commas(x$group_by), "\n", sep = "")
  if (length(x$order_by)) cat("Order by: ", named_commas(x$order_by), "\n", sep = "")
  if (length(x$having))   cat("Having:   ", named_commas(x$having), "\n", sep = "")
  if (length(x$limit))    cat("Limit:    ", x$limit, "\n", sep = "")
}


#' @export
#' @rdname sql_build
join_query <- function(x, y, vars, type = "inner", by = NULL, suffix = c(".x", ".y")) {
  structure(
    list(
      x = x,
      y = y,
      vars = vars,
      type = type,
      by = by
    ),
    class = c("join_query", "query")
  )
}


# Returns NULL if variables don't need to be renamed
join_vars <- function(x_names, y_names, by, suffix = c(".x", ".y")) {
  # Remove join keys from y
  y_names <- setdiff(y_names, by$y)

  # Add suffix where needed
  suffix <- check_suffix(suffix)
  common <- intersect(x_names, y_names)

  x_match <- match(common, x_names)
  x_new <- x_names
  x_new[x_match] <- paste0(x_names[x_match], suffix$x)

  y_match <- match(common, y_names)
  y_new <- y_names
  y_new[y_match] <- paste0(y_names[y_match], suffix$y)

  list(x = setNames(x_new, x_names), y = setNames(y_new, y_names))
}

semi_join_vars <- function(x_names, y_names) {
  all_names <- set_names(union(x_names, y_names))

  x_new <- all_names
  x_new[!all_names %in% x_names] <- NA

  y_new <- all_names
  y_new[!all_names %in% y_names] <- NA

  list(x = x_new, y = y_new)
}


get_join_xy_names <- function(by, uniques) {
  xy_by <- by$x[by$x == by$y]
  x_names <- uniques$x
  x_rename <- names(x_names) %in% xy_by
  names(x_names)[!x_rename] <- ""

  y_names <- uniques$y
  y_remove <- names(y_names) %in% xy_by
  y_names <- unname(y_names[!y_remove])

  c(x_names, y_names)
}


#' @export
print.join_query <- function(x, ...) {
  cat("<SQL JOIN (", toupper(x$type), ")>\n", sep = "")
  cat("By:   ", paste0(x$by$x, "-", x$by$y, collapse = ", "), "\n", sep = "")

  cat(named_rule("X"), "\n", sep = "")
  print(x$x$ops)
  cat(named_rule("Y"), "\n", sep = "")
  print(x$y$ops)
}

#' @export
#' @rdname sql_build
semi_join_query <- function(x, y, anti = FALSE, by = NULL) {
  structure(
    list(
      x = x,
      y = y,
      anti = anti,
      by = by
    ),
    class = c("semi_join_query", "query")
  )
}

#' @export
print.semi_join_query <- function(x, ...) {
  cat(
    "<SQL ",
    if (x$anti) "ANTI" else "SEMI", " JOIN>\n",
    sep = ""
  )
  cat("By:   ", paste0(x$by$x, "-", x$by$y, collapse = ", "), "\n", sep = "")

  cat(named_rule("X"), "\n", sep = "")
  print(x$x$ops)
  cat(named_rule("Y"), "\n", sep = "")
  print(x$y$ops)
}

#' @export
#' @rdname sql_build
set_op_query <- function(x, y, type = type) {
  structure(
    list(
      x = x,
      y = y,
      type = type
    ),
    class = c("set_op_query", "query")
  )
}

#' @export
print.set_op_query <- function(x, ...) {
  cat("<SQL ", x$type, ">\n", sep = "")

  cat(named_rule("X"), "\n", sep = "")
  print(x$x$ops)
  cat(named_rule("Y"), "\n", sep = "")
  print(x$y$ops)
}
