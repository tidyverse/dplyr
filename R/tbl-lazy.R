#' @noRd
#' @examples
#' x <- tbl_lazy_local(mtcars)
#' x %>% group_by(cyl) %>% summarise(n = n()) %>% filter(n > 10)
tbl_lazy_remote <- function(src, base) {
  make_tbl("lazy", ops = op_base_remote(src, base))
}
tbl_lazy_local <- function(df) {
  make_tbl("lazy", ops = op_base_local(df, env = parent.frame()))
}

#' @export
print.tbl_lazy <- function(df) {
  cat("Source: lazy\n")
  cat("Vars  : ", commas(op_vars(x$ops)), "\n", sep = "")
  cat("Groups: ", commas(op_grps(x$ops)), "\n", sep = "")
  cat("\n")

  print(df$ops)
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
group_by_.tbl_lazy <- function(.data, ..., .dots, add = FALSE) {
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
    vars <- db_query_fields(x$src$con, x)
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
  select_vars_(op_vars(x$x), x$xdots, include = op_grps(x$x))
}
#' @export
op_vars.op_rename <- function(x) {
  rename_vars(op_vars(x$x), dots)
}
#' @export
op_vars.op_summarise <- function(x) {
  c(op_grps(x$x), names(x$dots))
}
#' @export
op_vars.op_mutate <- function(x) {
  c(op_vars(x$x), names(x$x))
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
  op_var(x$x)
}

# op_grps -----------------------------------------------------------------

op_grps <- function(x) UseMethod("op_grps")
#' @export
op_grps.op_base <- function(x) character()
#' @export
op_grps.op_group_by <- function(x) {
  if (isTRUE(x$args$add)) {
    union(names(x$dots), op_grps(x$x))
  } else {
    names(x$dots)
  }
}
#' @export
op_grps.op_summarise <- function(x) drop_last(op_grps(x$x))
#' @export
op_grps.op_single <- function(x) op_grps(x$x)
#' @export
op_grps.op_double <- function(x) op_grps(x$x)




