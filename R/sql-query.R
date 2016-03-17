

# select_query ------------------------------------------------------------

#' @export
#' @rdname sql_build
select_query <- function(from,
                         select = sql("*"),
                         where = character(),
                         group_by = character(),
                         having = character(),
                         order_by = character(),
                         distinct = FALSE) {

  stopifnot(is.character(select))
  stopifnot(is.character(where))
  stopifnot(is.character(group_by))
  stopifnot(is.character(having))
  stopifnot(is.character(order_by))
  stopifnot(is.logical(distinct), length(distinct) == 1L)

  structure(
    list(
      from = from,
      select = select,
      where = where,
      group_by = group_by,
      having = having,
      order_by = order_by,
      distinct = distinct
    ),
    class = c("select_query", "query")
  )
}

#' @export
print.select_query <- function(x, ...) {
  cat("<SQL SELECT", if (x$distinct) " DISTINCT", ">\n", sep = "")
  cat("From:     ", x$from, "\n", sep = "")

  if (length(x$select))   cat("Select:   ", named_commas(x$select), "\n", sep = "")
  if (length(x$where))    cat("Where:    ", named_commas(x$where), "\n", sep = "")
  if (length(x$group_by)) cat("Group by: ", named_commas(x$group_by), "\n", sep = "")
  if (length(x$order_by)) cat("Order by: ", named_commas(x$order_by), "\n", sep = "")
  if (length(x$having))   cat("Having:   ", named_commas(x$having), "\n", sep = "")
}


join_query <- function(x, y, type = "inner", by = NULL, suffix = c(".x", ".y")) {
  structure(
    list(
      x = x,
      y = y,
      type = type,
      by = by,
      suffix = suffix
    ),
    class = c("join_query", "query")
  )
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
  cat("<SQL ", if (x$anti) "ANTI" else "SEMI", " JOIN>\n", sep = "")
  cat("By:   ", paste0(x$by$x, "-", x$by$y, collapse = ", "), "\n", sep = "")

  cat(named_rule("X"), "\n", sep = "")
  print(x$x$ops)
  cat(named_rule("Y"), "\n", sep = "")
  print(x$y$ops)
}

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
