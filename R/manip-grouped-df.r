# TODO:
# * make sure all preserve grouping structure (apart from summarise)
# * extract out grouped view component

#' Data manipulation for grouped data frames.
#'
#' @examples
#' data("baseball", package = "plyr")
#' players <- group_by(baseball, id)
#
#' filter(players, g == max(g))
#' summarise(players, g = mean(g))
#' head(mutate(players, cyear = year - min(year) + 1))
#' head(arrange(players, year))
#' head(select(players, id:team))
#'
#' @name manip_grouped_data_frame
NULL

#' @rdname manip_grouped_data_frame
#' @export
#' @method filter grouped_data_frame
filter.grouped_data_frame <- function(.data, ...) {
  conds <- dots(...)
  n <- nrow(.data$obj)
  p <- length(conds)

  # Create group-wise view of input data
  grp <- new.env(size = ncol(data), parent = parent.frame())
  get_input <- function(j) {
    force(j)
    function(v) {
      if (!missing(v)) stop("Immutable view")
      .subset2(.data$obj, j)[rows]
    }
  }
  for (name in names(.data$obj)) {
    makeActiveBinding(name, get_input(name), grp)
  }

  # Loop over each group
  out <- rep(NA, n)
  for (i in seq_along(.data$index)) {
    rows <- .data$index[[i]]
    r <- rep(TRUE, length(rows))
    for (j in seq_along(conds)) {
      r <- eval(conds[[j]], grp)
      r <- r & !is.na(r)
    }
    out[rows] <- r
  }

  .data$obj[out, , drop = FALSE]
}

#' @rdname manip_data_frame
#' @export
#' @method summarise grouped_data_frame
summarise.grouped_data_frame <- function(.data, ...) {
  data <- .data$obj
  groups <- .data$index

  calls <- named_dots(...)
  n <- length(groups)
  p <- length(calls)

  grp <- new.env(size = p, parent = parent.frame())
  get_input <- function(j) {
    force(j)
    function(v) {
      if (!missing(v)) stop("Immutable view")
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

  out <- vector("list", p)
  names(out) <- names(calls)

  for (j in seq_len(p)) {
    for (i in seq_len(n)) {
      rows <- groups[[i]]
      if (i == 1L) {
        # Run once to make vector of right type
        col <- eval(calls[[j]], grp)
        length(col) <- n

      } else {
        col[[i]] <- eval(calls[[j]], grp)
      }
    }

    out[[j]] <- col

    name <- names(calls)[[j]]
    makeActiveBinding(name, get_output(name), grp)
  }

  out <- c(.data$labels, out) # expensive operation
  as_df(out)
}

#' @rdname manip_data_frame
#' @export
#' @method mutate grouped_data_frame
mutate.grouped_data_frame <- function(.data, ...) {
  data <- .data$obj
  groups <- .data$index

  calls <- named_dots(...)
  n <- length(groups)
  p <- length(calls)

  grp <- new.env(size = p, parent = parent.frame())
  get_input <- function(j) {
    force(j)
    function(v) {
      if (!missing(v)) stop("Immutable view")
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

  out <- vector("list", p)
  names(out) <- names(calls)

  for (j in seq_len(p)) {
    for (i in seq_len(n)) {
      rows <- groups[[i]]
      if (i == 1L) {
        # Run mutate once to make vector of right type
        template <- eval(calls[[j]], grp)
        col <- template[1]
        length(col) <- nrow(data)

        col[rows] <- template
      } else {
        col[rows] <- eval(calls[[j]], grp)
      }
    }

    out[[j]] <- col

    name <- names(calls)[[j]]
    makeActiveBinding(name, get_output(name), grp)
  }

  cbind(data, as_df(out))
}

#' @rdname manip_data_frame
#' @export
#' @method arrange grouped_data_frame
arrange.grouped_data_frame <- function(.data, ...) {
  conds <- dots(...)
  n <- nrow(.data$obj)
  p <- length(conds)

  # Create group-wise view of input data
  grp <- new.env(size = ncol(data), parent = parent.frame())
  get_input <- function(j) {
    force(j)
    function(v) {
      if (!missing(v)) stop("Immutable view")
      .subset2(.data$obj, j)[rows]
    }
  }
  for (name in names(.data$obj)) {
    makeActiveBinding(name, get_input(name), grp)
  }

  order_call <- substitute(order(...))
  out <- numeric(n)
  for (i in seq_along(.data$index)) {
    rows <- .data$index[[i]]

    ord <- eval(order_call, grp)
    out[rows] <- rows[ord]
  }

  .data$obj[out, , drop = FALSE]
}

#' @rdname manip_data_frame
#' @export
#' @method select grouped_data_frame
select.grouped_data_frame <- function(.data, ...) {
  nm <- source_vars(.data)
  nm_env <- as.list(setNames(seq_along(nm), nm))

  idx <- unlist(lapply(dots(...), eval, nm_env, parent.frame()))
  select <- nm[idx]

  .data$obj <- .data$obj[, nm[idx], drop = FALSE]
  .data
}
