tbl_lazy <- function(df) {
  make_tbl("lazy", ops = op_base_local(df))
}

lazy_frame <- function(...) {
  tbl_lazy(data_frame(...))
}

#' @export
same_src.tbl_lazy <- function(x, y) {
  inherits(y, "tbl_lazy")
}

#' @export
tbl_vars.tbl_lazy <- function(x) {
  op_vars(x$ops)
}

#' @export
groups.tbl_lazy <- function(x) {
  lapply(group_vars(x), as.name)
}

#' @export
group_vars.tbl_lazy <- function(x) {
  op_grps(x$ops)
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

#' @export
filter.tbl_lazy <- function(.data, ...) {
  dots <- quos(...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("filter", .data, dots = dots)
}
#' @export
filter_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("filter", .data, dots = dots)
}

#' @export
arrange.tbl_lazy <- function(.data, ...) {
  dots <- quos(...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  names(dots) <- NULL

  add_op_single("arrange", .data, dots = dots)
}
#' @export
arrange_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(dots, caller_env(), ...)
  arrange(.data, !!! dots)
}

#' @export
select.tbl_lazy <- function(.data, ...) {
  dots <- quos(...)
  add_op_single("select", .data, dots = dots)
}
#' @export
select_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  add_op_single("select", .data, dots = dots)
}

#' @export
rename.tbl_lazy <- function(.data, ...) {
  dots <- quos(...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("rename", .data, dots = dots)
}
#' @export
rename_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("rename", .data, dots = dots)
}

#' @export
summarise.tbl_lazy <- function(.data, ...) {
  dots <- quos(...)
  add_op_single("summarise", .data, dots = dots)
}
#' @export
summarise_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("summarise", .data, dots = dots)
}

#' @export
mutate.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- quos(..., .named = TRUE)
  dots <- partial_eval(dots, vars = op_vars(.data))

  # For each expression, check if it uses any newly created variables.
  # If so, nest the mutate()
  used_vars <- lapply(dots, function(x) all_names(get_expr(x)))

  init <- 0L
  for (i in seq_along(dots)) {
    cur_idx <- inc_seq(init + 1L, i - 1L)
    new_vars <- names(dots)[cur_idx]

    if (any(used_vars[[i]] %in% new_vars)) {
      .data <- dplyr::add_op_single("mutate", .data, dots = dots[cur_idx])
      init <- i
    }
  }

  if (init != 0L) {
    dots <- dots[-inc_seq(1L, init - 1)]
  }
  add_op_single("mutate", .data, dots = dots)
}


#' @export
mutate_.tbl_lazy <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("mutate", .data, dots = dots)
}

#' @export
group_by.tbl_lazy <- function(.data, ..., add = FALSE) {
  dots <- quos(...)
  dots <- partial_eval(dots, vars = op_vars(.data))

  if (length(dots) == 0) {
    return(.data)
  }

  groups <- group_by_prepare(.data, .dots = dots, add = add)
  names <- map_chr(groups$groups, as_string)

  add_op_single("group_by",
    groups$data,
    dots = set_names(groups$groups, names),
    args = list(add = FALSE)
  )
}
#' @export
group_by_.tbl_lazy <- function(.data, ..., .dots = list(), add = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  group_by(.data, !!! dots, add = add)
}

#' @export
head.tbl_lazy <- function(x, n = 6L, ...) {
  add_op_single("head", x, args = list(n = n))
}

#' @export
ungroup.tbl_lazy <- function(x, ...) {
  add_op_single("ungroup", x)
}

#' @export
distinct.tbl_lazy <- function(.data, ..., .keep_all = FALSE) {
  dots <- quos(..., .named = TRUE)
  dots <- partial_eval(dots, vars = op_vars(.data))
  add_op_single("distinct", .data, dots = dots, args = list(.keep_all = .keep_all))
}
#' @export
distinct_.tbl_lazy <- function(.data, ..., .dots = list(), .keep_all = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  distinct(.data, !!! dots, .keep_all = .keep_all)
}


# Dual table verbs ------------------------------------------------------------

add_op_join <- function(x, y, type, by = NULL, copy = FALSE,
                        suffix = c(".x", ".y"),
                        auto_index = FALSE, ...) {
  by <- common_by(by, x, y)
  y <- auto_copy(
    x, y, copy,
    indexes = if (auto_index) list(by$y)
  )

  vars <- join_vars(op_vars(x), op_vars(y), type = type, by = by, suffix = suffix)

  x$ops <- op_double("join", x, y, args = list(
    vars = vars,
    type = type,
    by = by,
    suffix = suffix
  ))
  x
}

add_op_semi_join <- function(x, y, anti = FALSE, by = NULL, copy = FALSE,
                             auto_index = FALSE, ...) {
  by <- common_by(by, x, y)
  y <- auto_copy(
    x, y, copy,
    indexes = if (auto_index) list(by$y)
  )

  x$ops <- op_double("semi_join", x, y, args = list(
    anti = anti,
    by = by
  ))
  x
}

add_op_set_op <- function(x, y, type, copy = FALSE, ...) {
  y <- auto_copy(x, y, copy)

  if (inherits(x$src$con, "SQLiteConnection")) {
    # LIMIT only part the compound-select-statement not the select-core.
    #
    # https://www.sqlite.org/syntax/compound-select-stmt.html
    # https://www.sqlite.org/syntax/select-core.html

    if (inherits(x$ops, "op_head") || inherits(y$ops, "op_head")) {
      stop("SQLite does not support set operations on LIMITs", call. = FALSE)
    }
  }

  x$ops <- op_double("set_op", x, y, args = list(type = type))
  x
}

# Currently the dual table verbs are defined on tbl_sql, because the
# because they definitions are bit too tightly connected to SQL.
