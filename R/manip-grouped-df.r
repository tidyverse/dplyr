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
#' @name manip_grouped_data_frame
NULL

#' @rdname manip_grouped_data_frame
#' @export
#' @method filter grouped_data_frame
filter.grouped_data_frame <- function(.data, ...) {
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

  grouped_data_frame(
    data = .data$obj[out, , drop = FALSE],
    vars = .data$vars,
    name = .data$name
  )
}

#' @rdname manip_data_frame
#' @export
#' @method summarise grouped_data_frame
summarise.grouped_data_frame <- function(.data, ...) {
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
  data_frame_source(
    data = as_df(out),
    name = .data$name
  )
}

#' @rdname manip_data_frame
#' @export
#' @method mutate grouped_data_frame
mutate.grouped_data_frame <- function(.data, ...) {
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

  grouped_data_frame(
    data = cbind(.data$obj, as_df(out)),
    vars = .data$vars,
    name = .data$name
  )
}

#' @rdname manip_data_frame
#' @export
#' @method arrange grouped_data_frame
arrange.grouped_data_frame <- function(.data, ...) {
  order_call <- substitute(order(...))
  if (is.lazy(.data)) .data <- build_index(.data)
  v <- make_view(.data, parent.frame())

  out <- rep(NA, nrow(.data))
  for (i in seq_along(.data$index)) {
    rows <- v$set_group(i)
    ord <- v$eval(order_call)
    out[rows] <- rows[ord]
  }

  grouped_data_frame(
    data = .data$obj[out, , drop = FALSE],
    vars = .data$vars,
    name = .data$name
  )
}

#' @rdname manip_data_frame
#' @export
#' @method select grouped_data_frame
select.grouped_data_frame <- function(.data, ...) {
  nm <- source_vars(.data)
  nm_env <- as.list(setNames(seq_along(nm), nm))

  idx <- unlist(lapply(dots(...), eval, nm_env, parent.frame()))
  select <- nm[idx]

  grouped_data_frame(
    data = .data$obj[, nm[idx], drop = FALSE],
    vars = .data$vars,
    name = .data$name
  )
}
