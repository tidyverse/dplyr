#' A grouped data frame.
#'
#' The easiest way to create a grouped data frame is to call the \code{group_by}
#' method on a data frame or tbl: this will take care of capturing
#' the unevalated expressions for you.
#'
#' @keywords internal
#' @param data a tbl or data frame.
#' @param vars a list of quoted variables.
#' @param drop if \code{TRUE} preserve all factor levels, even those without
#'   data.
#' @export
grouped_df <- function(data, vars, drop = TRUE) {
  if (length(vars) == 0) {
    return(tbl_df(data))
  }

  assert_that(is.data.frame(data), is.list(vars), all(sapply(vars,is.name)), is.flag(drop))
  grouped_df_impl(data, unname(vars), drop)
}

#' @rdname grouped_df
#' @export
is.grouped_df <- function(x) inherits(x, "grouped_df")

#' @export
print.grouped_df <- function(x, ..., n = NULL, width = NULL) {
  cat("Source: local data frame ", dim_desc(x), "\n", sep = "")

  grps <- if (is.null(attr(x, "indices"))) "?" else length(attr(x, "indices"))
  cat("Groups: ", commas(deparse_all(groups(x))), " [", big_mark(grps), "]\n", sep = "")
  cat("\n")
  print(trunc_mat(x, n = n, width = width))
  invisible(x)
}

#' @export
group_size.grouped_df <- function(x) {
  group_size_grouped_cpp(x)
}

#' @export
n_groups.grouped_df <- function(x) {
  length(attr(x, "indices"))
}

#' @export
groups.grouped_df <- function(x) {
  attr(x, "vars")
}

#' @export
as.data.frame.grouped_df <- function(x, row.names = NULL,
                                     optional = FALSE, ...) {
  x <- ungroup(x)
  class(x) <- "data.frame"
  x
}

#' @export
ungroup.grouped_df <- function(x) {
  ungroup_grouped_df(x)
}

#' @export
`[.grouped_df` <- function(x, i, j, ...) {
  y <- NextMethod()

  group_vars <- vapply(groups(x), as.character, character(1))

  if (!all(group_vars %in% names(y))) {
    tbl_df(y)
  } else {
    grouped_df(y, groups(x))
  }

}

# One-table verbs --------------------------------------------------------------

#' @export
select_.grouped_df <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)

  vars <- select_vars_(names(.data), dots,
    include = as.character(groups(.data)))

  select_impl(.data, vars)
}

#' @export
rename_.grouped_df <- function(.data, ..., .dots) {
  dots <- lazyeval::all_dots(.dots, ...)
  vars <- rename_vars_(names(.data), dots)

  select_impl(.data, vars)
}


# Do ---------------------------------------------------------------------------


#' @export
do_.grouped_df <- function(.data, ..., env = parent.frame(), .dots) {
  # Force computation of indices
  if (is.null(attr(.data, "indices"))) {
    .data <- grouped_df_impl(.data, attr(.data, "vars"),
      attr(.data, "drop") %||% TRUE)
  }

  # Create ungroup version of data frame suitable for subsetting
  group_data <- ungroup(.data)

  args <- lazyeval::all_dots(.dots, ...)
  named <- named_args(args)
  env <- new.env(parent = lazyeval::common_env(args))
  labels <- attr(.data, "labels")

  index <- attr(.data, "indices")
  n <- length(index)
  m <- length(args)

  # Special case for zero-group/zero-row input
  if (n == 0) {
    env$. <- group_data

    if (!named) {
      out <- eval(args[[1]]$expr, envir = env)[0, , drop = FALSE]
      return(label_output_dataframe(labels, list(list(out)), groups(.data)))
    } else {
      out <- setNames(rep(list(list()), length(args)), names(args))
      return(label_output_list(labels, out, groups(.data)))
    }
  }

  # Create new environment, inheriting from parent, with an active binding
  # for . that resolves to the current subset. `_i` is found in environment
  # of this function because of usual scoping rules.
  makeActiveBinding(env = env, ".", function(value) {
    if (missing(value)) {
      group_data[index[[`_i`]] + 1L, , drop = FALSE]
    } else {
      group_data[index[[`_i`]] + 1L, ] <<- value
    }
  })

  out <- replicate(m, vector("list", n), simplify = FALSE)
  names(out) <- names(args)
  p <- progress_estimated(n * m, min_time = 2)

  for (`_i` in seq_len(n)) {
    for (j in seq_len(m)) {
      out[[j]][`_i`] <- list(eval(args[[j]]$expr, envir = env))
      p$tick()$print()
    }
  }

  if (!named) {
    label_output_dataframe(labels, out, groups(.data))
  } else {
    label_output_list(labels, out, groups(.data))
  }
}

# Set operations ---------------------------------------------------------------

#' @export
distinct_.grouped_df <- function(.data, ..., .dots) {
  groups <- lazyeval::as.lazy_dots(groups(.data))
  dist <- distinct_vars(.data, ..., .dots = c(.dots, groups))

  grouped_df(distinct_impl(dist$data, dist$vars), groups(.data))
}


# Random sampling --------------------------------------------------------------


#' @export
sample_n.grouped_df <- function(tbl, size, replace = FALSE, weight = NULL,
  .env = parent.frame()) {

  assert_that(is.numeric(size), length(size) == 1, size >= 0)
  weight <- substitute(weight)

  index <- attr(tbl, "indices")
  sampled <- lapply(index, sample_group, frac = FALSE,
    tbl = tbl, size = size, replace = replace, weight = weight, .env = .env)
  idx <- unlist(sampled) + 1

  grouped_df(tbl[idx, , drop = FALSE], vars = groups(tbl))
}

#' @export
sample_frac.grouped_df <- function(tbl, size = 1, replace = FALSE, weight = NULL,
  .env = parent.frame()) {

  assert_that(is.numeric(size), length(size) == 1, size >= 0)
  if (size > 1 && !replace) {
    stop("Sampled fraction can't be greater than one unless replace = TRUE",
      call. = FALSE)
  }
  weight <- substitute(weight)

  index <- attr(tbl, "indices")
  sampled <- lapply(index, sample_group, frac = TRUE,
    tbl = tbl, size = size, replace = replace, weight = weight, .env = .env)
  idx <- unlist(sampled) + 1

  grouped_df(tbl[idx, , drop = FALSE], vars = groups(tbl))
}

sample_group <- function(tbl, i, frac = FALSE, size, replace = TRUE,
  weight = NULL, .env = parent.frame()) {
  n <- length(i)
  if (frac) size <- round(size * n)

  check_size(size, n, replace)

  # weight use standard evaluation in this function
  if (!is.null(weight)) {
    weight <- eval(weight, tbl[i + 1, , drop = FALSE], .env)
    weight <- check_weight(weight, n)
  }

  i[sample.int(n, size, replace = replace, prob = weight)]
}


