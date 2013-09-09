#' Data manipulation for grouped data frames.
#'
#' Grouping status is preserved across filter, arrange, mutate and select.
#' Summarise produces an ungrouped data frame.
#'
#' @param .data a data frame
#' @param ... variables interpreted in the context of \code{.data}
#' @examples
#' by_dest <- group_by(hflights, Dest)
#
#' filter(by_dest, ArrDelay == max(ArrDelay))
#' summarise(by_dest, arr = mean(ArrDelay, na.rm = TRUE))
#' 
#' # Normalise arrival and departure delays by airport
#' scaled <- mutate(by_dest, arr_z = scale(ArrDelay), dep_z = scale(DepDelay))
#' select(scaled, Year:DayOfWeek, Dest, arr_z:dep_z)
#' 
#' arrange(by_dest, desc(ArrDelay))
#' select(by_dest, -(DayOfWeek:TailNum))
#'
#' # All manip functions preserve grouping structure, except for summarise
#' # which removes a grouping level
#' by_day <- group_by(hflights, Year, Month, DayofMonth)
#' by_month <- summarise(by_day, delayed = sum(ArrDelay > 0, na.rm = TRUE))
#' by_month
#' summarise(by_month, delayed = sum(delayed))
#'
#' # You can also manually ungroup:
#' ungroup(by_day)
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
  for (i in seq_along(attr(.data, "index"))) {
    rows <- v$set_group(i)

    r <- rep(TRUE, length(rows))
    for (j in seq_along(conds)) {
      r <- v$eval(conds[[j]])
      r <- r & !is.na(r)
    }
    out[rows] <- r
  }

  grouped_df(.data[out, , drop = FALSE], attr(.data, "vars"))
}

#' @rdname manip_grouped_df
#' @export
#' @method summarise grouped_df
summarise.grouped_df <- function(.data, ...) {
  calls <- named_dots(...)
  if (is.lazy(.data)) .data <- build_index(.data)
  v <- make_view(.data, parent.frame())
  v$add_function("n", function() length(rows))

  ngrps <- length(attr(.data, "index"))

  output_summary <- function(j) {
    force(j)
    function(v) {
      if (!missing(v)) stop("Immutable view")
      .subset2(out, j)[i]
    }
  }

  out_cols <- unique(names(calls))
  out <- setNames(vector("list", length(out_cols)), out_cols)

  for (j in seq_along(calls)) {
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
    out[[names(calls)[j]]] <- col

    name <- names(calls)[[j]]
    v$add_binding(name, output_summary(name))
  }
  
  out <- c(attr(.data, "labels"), out) # expensive operation
  
  grp_vars <- attr(.data, "vars")
  if (length(grp_vars) == 1L) {
    tbl_df(as_df(out))
  } else {
    grouped_df(as_df(out), grp_vars[-length(grp_vars)])
  }
}

#' @rdname manip_grouped_df
#' @export
#' @method mutate grouped_df
mutate.grouped_df <- function(.data, ...) {
  calls <- named_dots(...)
  if (is.lazy(.data)) .data <- build_index(.data)
  v <- make_view(.data, parent.frame())
  ngrps <- length(attr(.data, "index"))

  output_var <- function(j) {
    force(j)
    function(v) {
      if (!missing(v)) stop("Immutable view")
      .subset2(out, j)[rows]
    }
  }

  out_cols <- unique(names(calls))
  out <- setNames(vector("list", length(out_cols)), out_cols)

  for (j in seq_along(calls)) {
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

    out[[names(calls)[j]]] <- col

    name <- names(calls)[[j]]
    v$add_binding(name, output_var(name))
  }

  grouped_df(
    data = cbind(.data, as_df(out)),
    vars = attr(.data, "vars")
  )
}

#' @rdname manip_grouped_df
#' @export
#' @method arrange grouped_df
arrange.grouped_df <- function(.data, ...) {
  order_by <- unname(c(attr(.data, "var"), dots(...)))
  order_call <- as.call(c(list(quote(order)), order_by))

  out <- eval(order_call, .data, parent.frame())

  grouped_df(.data[out, , drop = FALSE], attr(.data, "vars"))
}

#' @rdname manip_grouped_df
#' @export
#' @method select grouped_df
select.grouped_df <- function(.data, ...) {
  input <- var_eval(dots(...), .data, parent.frame())

  grouped_df(.data[, input, drop = FALSE], attr(.data, "vars"))
}

#' @S3method do grouped_df
do.grouped_df <- function(.data, .f, ...) {
  if (is.lazy(.data)) .data <- build_index(.data)

  index <- attr(.data, "index")
  out <- vector("list", length(index))

  for (i in seq_along(index)) {
    subs <- .data[index[[i]], , drop = FALSE]
    out[[i]] <- .f(subs, ...)
  }

  out
}
