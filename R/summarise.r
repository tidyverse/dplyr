# Test with:
#  * dates and factors
#  * variables that depend on previous

#' Summarise by.
#'
#' Summarise variables in a data source broken down by some grouping variable.
#'
#' @section Restrictions:
#' \itemize{
#'  \item summary functions must return single value
#'  \item summary functions must return the same type of value for all groups
#' }
#' @param .source data source.  See \code{\link{source}} for more details.
#' @param .group grouping variable or variables. Either a character vector,
#'  a formula, or the output of \code{\link[plyr]{.}}.  Use \code{NULL} for
#'  no grouping.
#' @param ... additional named arguments are processed in the context of the
#'   \code{source}.
#' @param .env the environment in which to look for any additional variables
#'   supplied to the calls in \code{...}.
summarise_by <- function(.source, .group, ..., .n = 1e5, .env = parent.frame()) {
  if (is.data.frame(.source)) {
    source <- source_data_frame(.source, deparse(substitute(.source)))
  }
  stopifnot(is.source(.source))

  if (!is.quoted(.group)) {
    .group <- as.quoted(.group, env = .env)
  }

  calls <- named_dots(...)
  do_summarise_by(.source, .group, calls, n = .n, env = .env)
}

do_summarise_by <- function(source, group, calls, n = 1e5, env = parent.frame()) {
  UseMethod("do_summarise_by")
}

do_summarise_by.source_sqlite <- function(source, group, calls, n = 1e5, env = parent.frame()) {
  select <- vapply(calls, translate, source = source, env = env,
    FUN.VALUE = character(1))
  group_by <- vapply(group, translate, source = source, env = env,
    FUN.VALUE = character(1))

  sql_select(source,
    select = c(group_by, select),
    group_by = group_by,
    n = n
  )
}

do_summarise_by.source_data_table <- function(source, group, calls,
                                           env = parent.frame()) {
  by_call <- as.call(c(quote(list), group))
  list_call <- as.call(c(quote(list), calls))

  dt_call <- substitute(source$obj[, calls, by = by],
    list(calls = list_call, by = by_call))
  eval(dt_call)
}

do_summarise_by.source_data_frame <- function(source, group, calls, env = parent.frame()) {
  data <- source$obj
  groups <- group_ids(group, data)

  n <- length(groups)
  p <- length(calls)

  out <- vector("list", p)
  names(out) <- names(calls)

  grp <- new.env(size = p, parent = parent.frame())
  get_input <- function(j) {
    force(j)
    function(v) {
      if (!missing(v)) stop("Immutable view")
      # equivalent to data[[j]][rows] but avoids costly S3 dispatch
      .subset2(data, j)[rows]
    }
  }
  get_output <- function(j) {
    force(j)
    function(v) {
      if (!missing(v)) stop("Immutable view")
      .subset2(out, j)[i]
    }
  }
  for (name in names(data)) {
    makeActiveBinding(name, get_input(name), grp)
  }

  for (i in seq_len(n)) {
    rows <- groups[[i]]

    for (j in seq_len(p)) {
      if (i == 1L) {
        # Run summarise once to make vector of right type
        out[[j]] <- eval(calls[[j]], grp)
        length(out[[j]]) <- n

        name <- names(calls)[[i]]
        ## Need to think about enabling this: what happens when you do
        ## x = mean(x)
        # makeActiveBinding(name, get_output(name), grp)
      } else {
        out[[j]][[i]] <- eval(calls[[j]], grp)
      }
    }
  }

  # Coerce to data frame in place to avoid copying
  class(out) <- "data.frame"
  attr(out, "row.names") <- c(NA_integer_, -n)

  out
}

