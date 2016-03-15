#' @noRd
#' @examples
#' x <- tbl_lazy(mtcars)
#' x %>% group_by(cyl) %>% summarise(n = n()) %>% filter(n > 10)
tbl_lazy_remote <- function(src, base) {
  make_tbl("lazy", src = src, ops = op_base_remote(src, base))
}

tbl_lazy <- function(df) {
  make_tbl("lazy", ops = op_base_local(df, env = parent.frame()))
}

lazy_frame <- function(...) {
  tbl_lazy(data_frame(...))
}

#' @export
print.tbl_lazy <- function(x, ...) {
  cat("Source: lazy\n")
  cat("Vars  : ", commas(op_vars(x$ops)), "\n", sep = "")
  cat("Groups: ", commas(op_grps(x$ops)), "\n", sep = "")
  cat("\n")

  print(x$ops)
}

#' @export
filter_.tbl_lazy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  add_op_single(.data, "filter", dots = dots)
}

#' @export
arrange_.tbl_lazy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  add_op_single(.data, "arrange", dots = dots)
}

#' @export
select_.tbl_lazy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  add_op_single(.data, "select", dots = dots)
}

#' @export
rename_.tbl_lazy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  add_op_single(.data, "rename", dots = dots)
}

#' @export
summarise_.tbl_lazy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  add_op_single(.data, "summarise", dots = dots)
}

#' @export
mutate_.tbl_lazy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  add_op_single(.data, "mutate", dots = dots)
}

#' @export
group_by_.tbl_lazy <- function(.data, ..., .dots, add = TRUE) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  add_op_single(.data, "group_by", dots = dots, args = list(add = add))
}

#' @export
distinct_.tbl_lazy <- function(.data, ..., .dots, .keep_all = FALSE) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  add_op_single(.data, "distinct", dots = dots, args = list(.keep_all = .keep_all))
}

add_op_single <- function(.data, name, dots = list(), args = list()) {
  .data$ops <- op_single(name, x = .data$ops, dots = dots, args = args)
  .data
}

add_op_double <- function(.data, name, y, dots = list(), args = list()) {
  .data$ops <- op_double(name, x = .data$ops, y = y, dots = dots, args = args)
  .data
}

# S3 Ops classes -----------------------------------------------------------

op_base_remote <- function(src, x, vars = NULL) {
  if (is.null(vars)) {
    vars <- db_query_fields(src$con, x)
  }
  op_base("remote", src, x, vars)
}

#' @export
print.op_base_remote <- function(x, ...) {
  cat("Source: ", src_desc(x$src), "\n", sep = "")

  if (inherits(x$x, "ident")) {
    cat("From: ", x$x, "\n", sep = "")
  } else {
    cat("From: <derived table>\n")
  }

  cat("<Table: ", x$x, ">\n", sep = "")
}

op_base_local <- function(df, env = parent.frame()) {
  op_base("local", src_df(env = env), df, names(df))
}

#' @export
print.op_base_local <- function(x, ...) {
  cat("<Local data frame> ", dim_desc(x$x), "\n", sep = "")
}

op_base <- function(name, src, x, vars) {
  stopifnot(is.character(vars))

  structure(
    list(
      src = src,
      x = x,
      vars = vars
    ),
    class = c(paste0("op_base_", name), "op_base", "op")
  )

}


op_single <- function(name, x, dots = list(), args = list()) {
  structure(
    list(
      name = name,
      x = x,
      dots = dots,
      args = args
    ),
    class = c(paste0("op_", name), "op_single", "op")
  )
}

#' @export
print.op_single <- function(x, ...) {
  print(x$x)

  cat("-> ", x$name, "()\n", sep = "")
  for (dot in x$dots) {
    cat("   - ", deparse_trunc(dot$expr), "\n", sep = "")
  }
}

op_double <- function(name, x, y, dots = list(), args = list()) {
  structure(
    list(
      name = name,
      x = x,
      y = y,
      dots = dots,
      args = args
    ),
    class = c(paste0("op_", name), "op_double", "op")
  )
}

# op_vars -----------------------------------------------------------------

op_vars <- function(x) UseMethod("op_vars")

#' @export
op_vars.op_base <- function(x) {
  x$vars
}
#' @export
op_vars.op_select <- function(x) {
  names(select_vars_(op_vars(x$x), x$dots, include = op_grps(x$x)))
}
#' @export
op_vars.op_rename <- function(x) {
  names(rename_vars_(op_vars(x$x), x$dots))
}
#' @export
op_vars.op_summarise <- function(x) {
  c(op_grps(x$x), names(x$dots))
}
#' @export
op_vars.op_mutate <- function(x) {
  c(op_vars(x$x), names(x$dots))
}
#' @export
op_vars.op_single <- function(x) {
  op_vars(x$x)
}
#' @export
op_vars.op_join <- function(x) {
  unique_names(op_vars(x$x), op_vars(x$x), by = x$args$by, suffix = x$args$suffix)
}
#' @export
op_vars.op_semi_join <- function(x) {
  op_vars(x$x)
}
#' @export
op_vars.op_anti_join <- function(x) {
  op_vars(x$x)
}
#' @export
op_vars.op_set <- function(x) {
  op_vars(x$x)
}

#' @export
op_vars.tbl_lazy <- function(x) {
  op_vars(x$ops)
}

# op_grps -----------------------------------------------------------------

op_grps <- function(x) UseMethod("op_grps")
#' @export
op_grps.op_base <- function(x) character()
#' @export
op_grps.op_group_by <- function(x) {
  if (isTRUE(x$args$add)) {
    union(op_grps(x$x), names(x$dots))
  } else {
    names(x$dots)
  }
}
#' @export
op_grps.op_summarise <- function(x) {
  grps <- op_grps(x$x)
  if (length(grps) == 1) {
    character()
  } else {
    grps[-length(grps)]
  }
}
#' @export
op_grps.op_single <- function(x) {
  op_grps(x$x)
}
#' @export
op_grps.op_double <- function(x) {
  op_grps(x$x)
}

#' @export
op_grps.tbl_lazy <- function(x) {
  op_grps(x$ops)
}


# sql_build ---------------------------------------------------------------

#' Build and render SQL from a sequence of lazy operations
#'
#' Build creates a \code{select_query} S3 object, and then \code{sql_render}
#' converts it into a SQL string.
#'
#' @export
#' @keywords internal
sql_build <- function(op, ...) {
  UseMethod("sql_build")
}

#' @export
sql_build.tbl_lazy <- function(op, ...) {
  sql_build(op$ops, ...)
}

#' @export
sql_build.op_base_remote <- function(op, ...) {
  op$x
}

#' @export
sql_build.op_base_local <- function(op, ...) {
  ident("df")
}

#' @export
sql_build.op_select <- function(op, ...) {
  vars <- select_vars_(op_vars(op$x), op$dots, include = op_grps(op$x))
  select_query(sql_build(op$x), vars)
}

#' @export
sql_build.op_rename <- function(op, ...) {
  vars <- rename_vars_(op_vars(op$x), op$dots)
  select_query(sql_build(op$x, con), vars)
}

#' @export
#' @rdname sql_build
select_query <- function(from, select,
                         where = character(),
                         group_by = character(),
                         having = character(),
                         order_by = character(),
                         limit = NULL,
                         offset = NULL) {

  stopifnot(is.character(select))
  stopifnot(is.character(where))
  stopifnot(is.character(group_by))
  stopifnot(is.character(having))
  stopifnot(is.character(order_by))
  stopifnot(is.null(limit) || (is.integer(limit) && length(limit) == 1L))
  stopifnot(is.null(offset) || (is.integer(offset) && length(offset) == 1L))

  structure(
    list(
      from = from,
      select = select,
      where = where,
      group_by = group_by,
      having = having,
      limit = limit,
      offset = offset
    ),
    class = "select_query"
  )
}

#' @export
print.select_query <- function(x, ...) {
  cat("<SQL SELECT>\n")
  cat("From:     ", x$from, "\n", sep = "")

  if (length(x$select))   cat("Select:   ", named_commas(x$select), "\n", sep = "")
  if (length(x$where))    cat("Where:    ", named_commas(x$where), "\n", sep = "")
  if (length(x$group_by)) cat("Group by: ", named_commas(x$group_by), "\n", sep = "")
  if (length(x$having))   cat("Having:   ", named_commas(x$having), "\n", sep = "")
}

#' @export
#' @rdname sql_build
sql_render <- function(x, con = NULL, ...) {
  UseMethod("sql_render")
}

#' @export
sql_render.tbl_lazy <- function(op, con = NULL, ...) {
  sql_render(sql_build(op$ops, ...), con = con, ...)
}

#' @export
sql_render.select_query <- function(x, con = NULL, ...) {
  from <- sql_subquery(con, sql_render(x$from, con))

  sql_select(
    con, x$select, from, where = x$where, group_by = x$group_by,
    having = x$having, order_by = x$order_by, limit = x$limit,
    offset = x$offset, ...
  )
}

#' @export
sql_render.ident <- function(x, con = NULL, ...) {
  x
}

#' @export
sql_render.sql <- function(x, con = NULL, ...) {
  x
}
