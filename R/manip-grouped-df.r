#' Data manipulation for grouped data frames.
#'
#' Grouping status is preserved across filter, arrange, mutate and select.
#' Summarise produces an ungrouped data frame.
#'
#' @examples
#' data("baseball", package = "plyr")
#' players <- group_by(baseball, id, name = "players")
#
#' filter(players, g == max(g))
#' summarise(players, g = mean(g))
#' mutate(players, cyear = year - min(year) + 1)
#' arrange(players, id, desc(year))
#' select(players, id:team)
#'
#' # All manip functions preserve grouping structure, except for summarise
#' # (for hopefully obvious reasons)
#' by_year <- mutate(players, cyear = year - min(year) + 1)
#' summarise(by_year, years = max(cyear))
#'
#' # You can also manually ungroup:
#' arrange(ungroup(by_year), id, year)
#'
#' @name manip_grouped_df
NULL

#' @rdname manip_grouped_df
#' @export
#' @method filter grouped_df
filter.grouped_df <- function(.data, ...) {
  conds <- dots(...)
  if (is.lazy(.data)) .data <- build_index(.data)
  v <- make_view(.data, parent.frame())

  out <- rep(NA, nrow(.data))
  for (i in seq_along(.data$index)) {
    rows <- v$set_group(i)

    r <- rep(TRUE, length(rows))
    for (j in seq_along(conds)) {
      r <- v$eval(conds[[j]])
      r <- r & !is.na(r)
    }
    out[rows] <- r
  }

  grouped_df(
    data = .data$obj[out, , drop = FALSE],
    vars = .data$vars,
    name = .data$name
  )
}

#' @rdname manip_data_frame
#' @export
#' @method summarise grouped_df
summarise.grouped_df <- function(.data, ...) {
  calls <- named_dots(...)
  if (is.lazy(.data)) .data <- build_index(.data)
  v <- make_view(.data, parent.frame())
  ngrps <- length(.data$index)

  output_summary <- function(j) {
    force(j)
    function(v) {
      if (!missing(v)) stop("Immutable view")
      .subset2(out, j)[i]
    }
  }

  out <- vector("list", length(calls))
  names(out) <- names(calls)

  for (j in seq_along(out)) {
    for (i in seq_len(ngrps)) {
      rows <- v$set_group(i)
      if (i == 1L) {
        # Run once to make vector of right type
        col <- v$eval(calls[[j]])
        length(col) <- ngrps

      } else {
        col[[i]] <- v$eval(calls[[j]])
      }
    }
    out[[j]] <- col

    name <- names(calls)[[j]]
    v$add_binding(name, output_summary(name))
  }

  out <- c(.data$labels, out) # expensive operation
  source_df(
    data = as_df(out),
    name = .data$name
  )
}

#' @rdname manip_data_frame
#' @export
#' @method mutate grouped_df
mutate.grouped_df <- function(.data, ...) {
  calls <- named_dots(...)
  if (is.lazy(.data)) .data <- build_index(.data)
  v <- make_view(.data, parent.frame())
  ngrps <- length(.data$index)

  output_var <- function(j) {
    force(j)
    function(v) {
      if (!missing(v)) stop("Immutable view")
      .subset2(out, j)[rows]
    }
  }

  out <- vector("list", length(calls))
  names(out) <- names(calls)

  for (j in seq_along(out)) {
    for (i in seq_len(ngrps)) {
      rows <- v$set_group(i)
      if (i == 1L) {
        # Run mutate once to make vector of right type
        template <- v$eval(calls[[j]])
        col <- template[1]
        length(col) <- nrow(.data)

        col[rows] <- template
      } else {
        col[rows] <- v$eval(calls[[j]])
      }
    }

    out[[j]] <- col

    name <- names(calls)[[j]]
    v$add_binding(name, output_var(name))
  }

  grouped_df(
    data = cbind(.data$obj, as_df(out)),
    vars = .data$vars,
    name = .data$name
  )
}

#' @rdname manip_data_frame
#' @export
#' @method arrange grouped_df
arrange.grouped_df <- function(.data, ...) {
  order_call <- substitute(order(...))
  if (is.lazy(.data)) .data <- build_index(.data)
  v <- make_view(.data, parent.frame())

  out <- rep(NA, nrow(.data))
  for (i in seq_along(.data$index)) {
    rows <- v$set_group(i)
    ord <- v$eval(order_call)
    out[rows] <- rows[ord]
  }

  grouped_df(
    data = .data$obj[out, , drop = FALSE],
    vars = .data$vars,
    name = .data$name
  )
}

#' @rdname manip_data_frame
#' @export
#' @method select grouped_df
select.grouped_df <- function(.data, ...) {
  input <- var_eval(.data, dots(...), parent.frame())

  grouped_df(
    data = .data$obj[, input, drop = FALSE],
    vars = .data$vars,
    name = .data$name
  )
}
