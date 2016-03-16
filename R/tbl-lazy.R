#' @noRd
#' @examples
#' x <- tbl_lazy(mtcars)
#' x %>% group_by(cyl) %>% summarise(n = n()) %>% filter(n > 10)

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

# Single table methods ----------------------------------------------------

add_op_single <- function(name, .data, dots = list(), args = list()) {
  .data$ops <- op_single(name, x = .data$ops, dots = dots, args = args)
  .data
}

#' @export
filter_.tbl_lazy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  add_op_single("filter", .data, dots = dots)
}

#' @export
arrange_.tbl_lazy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  add_op_single("arrange", .data, dots = dots)
}

#' @export
select_.tbl_lazy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  add_op_single("select", .data, dots = dots)
}

#' @export
rename_.tbl_lazy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  add_op_single("rename", .data, dots = dots)
}

#' @export
summarise_.tbl_lazy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  add_op_single("summarise", .data, dots = dots)
}

#' @export
mutate_.tbl_lazy <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  add_op_single("mutate", .data, dots = dots)
}

#' @export
group_by_.tbl_lazy <- function(.data, ..., .dots, add = TRUE) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  add_op_single("group_by", .data, dots = dots, args = list(add = add))
}

#' @export
ungroup.tbl_lazy <- function(x, ...) {
  add_op_single("ungroup", x)
}

#' @export
distinct_.tbl_lazy <- function(.data, ..., .dots, .keep_all = FALSE) {
  dots <- lazyeval::all_dots(.dots, ..., all_named = TRUE)
  add_op_single("distinct", .data, dots = dots, args = list(.keep_all = .keep_all))
}
