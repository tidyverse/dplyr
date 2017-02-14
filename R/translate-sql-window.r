#' Generate SQL expression for window functions
#'
#' `win_over()` makes it easy to generate the window function specification.
#' `win_absent()`, `win_rank()`, `win_recycled()`, and `win_cumulative()`
#' provide helpers for constructing common types of window functions.
#' `win_current_group()` and `win_current_order()` allow you to access
#' the grouping and order context set up by [group_by()] and [arrange()].
#'
#' @param expr The window expression
#' @param parition Variables to partition over
#' @param order Variables to order by
#' @param frame A numeric vector of length two defining the frame.
#' @param f The name of an sql function as a string
#' @export
#' @keywords internal
#' @examples
#' win_over("avg(x)")
#' win_over("avg(x)", "y")
#' win_over("avg(x)", order = "y")
#' win_over("avg(x)", order = c("x", "y"))
#' win_over("avg(x)", frame = c(-Inf, 0), order = "y")
win_over <- function(expr, partition = NULL, order = NULL, frame = NULL) {
  if (length(partition) == 0) {
    partition <- NULL
  }
  if (!is.null(partition)) {
    if (!is.sql(partition)) {
      partition <- ident(partition)
    }

    partition <- build_sql(
      "PARTITION BY ",
      sql_vector(
        escape(partition, con = win_current_con()),
        collapse = ", ",
        parens = FALSE
      )
    )
  }
  if (!is.null(order)) {
    if (!is.sql(order)) {
      order <- ident(order)
    }

    order <- build_sql(
      "ORDER BY ",
      sql_vector(escape(order), collapse = ", ", parens = FALSE)
    )
  }
  if (!is.null(frame)) {
    if (is.null(order)) {
      warning(
        "Windowed expression '", expr, "' does not have explicit order.\n",
        "Please use arrange() to make determinstic.",
        call. = FALSE
      )
    }

    if (is.numeric(frame)) frame <- rows(frame[1], frame[2])
    frame <- build_sql("ROWS ", frame)
  }

  over <- sql_vector(compact(list(partition, order, frame)), parens = TRUE)
  sql <- build_sql(expr, " OVER ", over)

  sql
}

rows <- function(from = -Inf, to = 0) {
  if (from >= to) stop("from must be less than to", call. = FALSE)

  dir <- function(x) if (x < 0) "PRECEDING" else "FOLLOWING"
  val <- function(x) if (is.finite(x)) as.integer(abs(x)) else "UNBOUNDED"
  bound <- function(x) {
    if (x == 0) return("CURRENT ROW")
    paste(val(x), dir(x))
  }

  if (to == 0) {
    sql(bound(from))
  } else {
    sql(paste0("BETWEEN ", bound(from), " AND ", bound(to)))
  }
}


#' @rdname win_over
#' @export
win_rank <- function(f) {
  force(f)
  function(order = NULL) {
    win_over(build_sql(sql(f), list()), win_current_group(), order %||% win_current_order())
  }
}

#' @rdname win_over
#' @export
win_recycled <- function(f) {
  force(f)
  function(x) {
    win_over(build_sql(sql(f), list(x)), win_current_group())
  }
}

#' @rdname win_over
#' @export
win_cumulative <- function(f) {
  force(f)
  function(x) {
    win_over(
      build_sql(sql(f), list(x)),
      win_current_group(),
      win_current_order(),
      frame = c(-Inf, 0)
    )
  }
}

#' @rdname win_over
#' @export
win_absent <- function(f) {
  force(f)

  function(...) {
    stop(
      "Window function `", f, "()` is not supported by this database",
      call. = FALSE
    )
  }
}


# API to set default partitioning etc -------------------------------------

# Use a global variable to communicate state of partitioning between
# tbl and sql translator. This isn't the most amazing design, but it keeps
# things loosely coupled and is easy to understand.
partition <- new.env(parent = emptyenv())
partition$group_by <- NULL
partition$order_by <- NULL
partition$con <- NULL

set_win_current_con <- function(con) {
  old <- partition$con
  partition$con <- con
  invisible(old)
}

set_win_current_group <- function(vars) {
  stopifnot(is.null(vars) || is.character(vars))

  old <- partition$group_by
  partition$group_by <- vars
  invisible(old)
}

set_win_current_order <- function(vars) {
  stopifnot(is.null(vars) || is.character(vars))

  old <- partition$order_by
  partition$order_by <- vars
  invisible(old)
}

set_partition <- function(group_by, order_by, con = NULL) {
  old <- list(partition$group_by, partition$order_by)
  if (is.list(group_by)) {
    order_by <- group_by[[2]]
    group_by <- group_by[[1]]
  }

  partition$group_by <- group_by
  partition$order_by <- order_by
  partition$con <- con

  invisible(old)
}

#' @export
#' @rdname win_over
win_current_group <- function() partition$group_by

#' @export
#' @rdname win_over
win_current_order <- function() partition$order_by

# Not exported, because you shouldn't need it
win_current_con <- function() partition$con


# Where translation -------------------------------------------------------


uses_window_fun <- function(x, con) {
  if (is.null(x)) return(FALSE)
  if (is.list(x)) {
    calls <- unlist(lapply(x, all_calls))
  } else {
    calls <- all_calls(x)
  }

  win_f <- ls(envir = sql_translate_env(con)$window)
  any(calls %in% win_f)
}

common_window_funs <- function() {
  ls(sql_translate_env(NULL)$window)
}

#' @noRd
#' @examples
#' translate_window_where(quote(1))
#' translate_window_where(quote(x))
#' translate_window_where(quote(x == 1))
#' translate_window_where(quote(x == 1 && y == 2))
#' translate_window_where(quote(n() > 10))
#' translate_window_where(quote(rank() > cumsum(AB)))
translate_window_where <- function(expr, window_funs = common_window_funs()) {
  if (is.atomic(expr) || is.name(expr)) {
    window_where(expr, list())
  } else if (is.call(expr)) {
    if (as.character(expr[[1]]) %in% window_funs) {
      name <- unique_name()
      window_where(as.name(name), setNames(list(expr), name))
    } else {
      args <- lapply(expr[-1], translate_window_where, window_funs = window_funs)
      expr <- as.call(c(expr[[1]], lapply(args, "[[", "expr")))

      window_where(
        expr = expr,
        comp = unlist(lapply(args, "[[", "comp"), recursive = FALSE)
      )
    }
  } else {
    stop("Unknown type: ", typeof(expr))
  }
}


#' @noRd
#' @examples
#' translate_window_where_all(list(quote(x == 1), quote(n() > 2)))
#' translate_window_where_all(list(quote(cumsum(x) == 10), quote(n() > 2)))
translate_window_where_all <- function(x, window_funs = common_window_funs()) {
  out <- lapply(x, translate_window_where, window_funs = window_funs)

  list(
    expr = unlist(lapply(out, "[[", "expr"), recursive = FALSE),
    comp = unlist(lapply(out, "[[", "comp"), recursive = FALSE)
  )
}

window_where <- function(expr, comp) {
  stopifnot(is.call(expr) || is.name(expr) || is.atomic(expr))
  stopifnot(is.list(comp))

  list(
    expr = expr,
    comp = comp
  )
}
