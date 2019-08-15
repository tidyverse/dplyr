#' Create a data frame tbl.
#'
#' Deprecated: please use [tibble::as_tibble()] instead.
#'
#' @export
#' @keywords internal
#' @param data a data frame
tbl_df <- function(data) {
  # Works in tibble < 1.5.0 too, because .name_repair will be
  # swallowed by the ellipsis
  as_tibble(data, .name_repair = "check_unique")
}

#' @export
as.tbl.data.frame <- function(x, ...) {
  tbl_df(x)
}

#' @export
tbl_vars.data.frame <- function(x) {
  names(x)
}

#' @export
same_src.data.frame <- function(x, y) {
  is.data.frame(y)
}

#' @export
auto_copy.tbl_df <- function(x, y, copy = FALSE, ...) {
  as.data.frame(y)
}

# Verbs ------------------------------------------------------------------------

arrange_data_frame <- function(.data, ..., .by_group = FALSE) {
  if (dots_n(...) == 0L) {
    return(.data)
  }

  if (.by_group) {
    dots <- c(quos(!!!groups(.data)), enquos(...))
  } else {
    dots <- enquos(...)
  }

  directions <- map_chr(dots, function(quosure) {
    if(quo_is_call(quosure, "desc")) "desc" else "asc"
  })

  quosures <- map(dots, function(quosure) {
    if (quo_is_call(quosure, "desc")) {
      quosure <- new_quosure(
        node_cadr(quo_get_expr(quosure)),
        quo_get_env(quosure)
      )
    }
    quosure
  })
  # give the quosures arbitrary names so that
  # data has the right number of columns below after transmute()
  names(quosures) <- paste0("^^--arrange_quosure_", seq_along(quosures))

  # TODO: not quite that because when the quosure is some expression
  #       it should be evaluated by groups.
  #       for now this abuses transmute so that we get just one
  #       column per quosure
  #
  #       revisit when we have something like mutate_one() to
  #       evaluate one quosure in the data mask
  data <- transmute(as_tibble(.data), !!!quosures)

  # we can't just use vec_compare_proxy(data) because we need to apply
  # direction for each column, so we get a list of proxies instead
  # and then mimic vctrs:::order_proxy
  #
  # should really be map2(quosures, directions, ...)
  proxies <- map2(unname(data), directions, function(column, direction) {
    proxy <- vec_proxy_compare(column, relax = TRUE)
    desc <- identical(direction, "desc")
    if (is.data.frame(proxy)) {
      proxy <- order(vec_order(proxy,
        direction = direction,
        na_value = if(desc) "smallest" else "largest"
      ))
    } else if(desc) {
      proxy <- desc(proxy)
    }
    proxy
  })

  orders <- exec("order", !!!proxies, decreasing = FALSE, na.last = TRUE)

  out <- vec_slice(as_tibble(.data), orders)
  if (is_grouped_df(.data)) {
    out <- grouped_df(out, vars = group_vars(.data), drop = group_by_drop_default(.data))
  }
  out
}

#' @param .by_group If `TRUE`, will sort first by grouping variable. Applies to
#'   grouped data frames only.
#' @rdname arrange
#' @export
arrange.tbl_df <- function(.data, ..., .by_group = FALSE) {
  arrange_data_frame(.data, ..., .by_group = .by_group)
}

#' @export
arrange_.tbl_df <- function(.data, ..., .dots = list(), .by_group = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  arrange_data_frame(.data, !!!dots, .by_group = .by_group)
}

regroup <- function(data) {
  # only keep the non empty groups
  non_empty <- map_lgl(group_rows(data), function(.x) length(.x) > 0)
  gdata <- filter(group_data(data), non_empty)

  # then group the grouping data to get expansion if needed
  gdata <- grouped_df(gdata, head(names(gdata), -1L), isTRUE(attr(group_data(data), ".drop")))
  new_groups <- group_data(gdata)
  old_rows  <- gdata$.rows

  new_groups$.rows <- new_list_of(map(new_groups$.rows, function(.x) {
    if (length(.x) == 1L) {
      old_rows[[.x]]
    } else {
      integer()
    }
  }), ptype = integer())

  attr(data, "groups") <- new_groups
  data
}


#' @export
filter.tbl_df <- function(.data, ..., .preserve = FALSE) {
  dots <- enquos(...)
  if (any(have_name(dots))) {
    bad <- dots[have_name(dots)]
    bad_eq_ops(bad, "Filter specifications must not be named")
  } else if (is_empty(dots)) {
    return(.data)
  }

  quo <- all_exprs(!!!dots, .vectorised = TRUE)
  out <- filter_impl(.data, quo)
  if (!.preserve && is_grouped_df(.data)) {
    out <- regroup(out)
  }
  out
}
#' @export
filter_.tbl_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  filter(.data, !!!dots)
}

#' @export
slice.tbl_df <- function(.data, ..., .preserve = FALSE) {
  dots <- enquos(...)
  if (is_empty(dots)) {
    return(.data)
  }

  quo <- quo(c(!!!dots))
  out <- slice_impl(.data, quo)
  if (!.preserve && is_grouped_df(.data)) {
    out <- regroup(out)
  }
  out
}
#' @export
slice_.tbl_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  slice(.data, !!!dots)
}

#' @export
mutate.tbl_df <- function(.data, ...) {
  dots <- enquos(..., .named = TRUE)
  mutate_impl(.data, dots, caller_env())
}
#' @export
mutate_.tbl_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ..., .named = TRUE)
  mutate_impl(.data, dots, caller_env())
}

validate_summarise_sizes <- function(x, .size) {
  # https://github.com/r-lib/vctrs/pull/539
  sizes <- map_int(x, vec_size)
  if (any(sizes != .size)) {
    abort("Result does not respect vec_size() == .size")
  }
}

summarise_data_mask <- function(data, rows) {
  chunks_env <- env()

  if (inherits(data, "rowwise_df")) {
    # approximation for now
    map2(data, names(data), function(col, nm) {
      if (is_list(col) && !is.data.frame(col)) {
        env_bind_lazy(chunks_env, !!nm := map(rows, function(row) vec_slice(col, row)[[1L]]))
      } else {
        env_bind_lazy(chunks_env, !!nm := map(rows, vec_slice, x = col))
      }
    })
  } else {
    map2(data, names(data), function(col, nm) {
      env_bind_lazy(chunks_env, !!nm := map(rows, vec_slice, x = col))
    })
  }

  bottom <- env()
  column_names <- set_names(names(data))

  .current_group_index <- NA_integer_
  env_bind_active(bottom, !!!map(column_names, function(column) {
    function() {
      chunks_env[[column]][[.current_group_index]]
    }
  }))

  mask <- new_data_mask(bottom)
  mask$.set_current_group <- function(group_index) {
    .current_group_index <<- group_index
  }
  mask$.add_summarised <- function(name, chunks) {
    if (name %in% group_vars(data)) {
      abort(glue("Column `{name}` can't be modified because it's a grouping variable"))
    }
    env_bind_active(bottom, !!name := function() {
      chunks[[.current_group_index]]
    })
  }
  mask$.data <- as_data_pronoun(mask)

  mask
}

#' @importFrom tibble add_column
#' @export
summarise.tbl_df <- function(.data, ...) {
  dots <- enquos(...)
  dots_names <- names(dots)

  rows <- group_rows(.data)
  mask <- summarise_data_mask(.data, rows)
  caller <- caller_env()

  summaries <- list()

  .size <- 1L

  old_group_size <- context_env[["..group_size"]]
  old_group_number <- context_env[["..group_number"]]
  on.exit({
    context_env[["..group_size"]] <- old_group_size
    context_env[["..group_number"]] <- old_group_number
  })

  # workaround when there are 0 groups
  if (length(rows) == 0L) {
    rows <- list(integer(0))
  }

  for (i in seq_along(dots)) {
    # a list in which each element is the result of
    # evaluating the quosure in the "sliced data mask"
    #
    # TODO: reinject hybrid evaluation at the R level
    chunks <- map2(seq_along(rows), lengths(rows), function(group, n) {
      mask$.set_current_group(group)
      context_env[["..group_size"]] <- n
      context_env[["..group_number"]] <- group
      eval_tidy(dots[[i]], mask, env = caller)
    })

    ok <- all(map_lgl(chunks, vec_is))
    if (!ok) {
      if (is.null(dots_names) || dots_names[i] == "") {
        abort(glue("Unsupported type at index {i}"))
      } else {
        abort(glue("Unsupported type for result `{dots_names[i]}`"))
      }
    }

    if (identical(.size, 1L)) {
      sizes <- map_int(chunks, vec_size)
      if (any(sizes != 1L)) {
        .size <- sizes
      }
    } else {
      validate_summarise_sizes(chunks, .size)
    }

    result <- vec_c(!!!chunks)

    if (is.null(dots_names) || dots_names[i] == "") {
      # auto splice when the quosure is not named
      if (is.data.frame(result)) {
        summaries[names(result)] <- result

        # remember each result separately
        map2(seq_along(result), names(result), function(i, nm) {
          mask$.add_summarised(nm, map(chunks, i))
        })
      } else {
        abort("cannot auto splice non data frame results")
      }
    } else {
      # treat as a single output otherwise
      summaries[[ dots_names[i] ]] <-  result

      # remember
      mask$.add_summarised(dots_names[i], chunks)
    }

  }

  grouping <- group_keys(.data)
  if (!identical(.size, 1L)) {
    grouping <- vec_slice(grouping, rep(seq2(1L, nrow(grouping)), .size))
  }

  out <- add_column(grouping, !!!summaries)

  if (is_grouped_df(.data) && length(group_vars(.data)) > 1) {
    out <- grouped_df(out, head(group_vars(.data), -1), group_by_drop_default(.data))
  }

  # copy back attributes
  # TODO: challenge that with some vctrs theory
  atts <- attributes(.data)
  atts <- atts[! names(atts) %in% c("names", "row.names", "groups", "class")]
  for(name in names(atts)) {
    attr(out, name) <- atts[[name]]
  }

  out
}

#' @export
summarise_.tbl_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ..., .named = TRUE)
  summarise(.data, !!!dots)
}

# Joins ------------------------------------------------------------------------

#' Join data frame tbls
#'
#' See [join] for a description of the general purpose of the
#' functions.
#'
#' @inheritParams inner_join
#' @param ... included for compatibility with the generic; otherwise ignored.
#' @param na_matches
#'   Use `"never"` to always treat two `NA` or `NaN` values as
#'   different, like joins for database sources, similarly to
#'   `merge(incomparables = FALSE)`.
#'   The default, `"na"`, always treats two `NA` or `NaN` values as equal, like [merge()].
#'   Users and package authors can change the default behavior by calling
#'   `pkgconfig::set_config("dplyr::na_matches" = "never")`.
#' @examples
#' if (require("Lahman")) {
#' batting_df <- tbl_df(Batting)
#' person_df <- tbl_df(Master)
#'
#' uperson_df <- tbl_df(Master[!duplicated(Master$playerID), ])
#'
#' # Inner join: match batting and person data
#' inner_join(batting_df, person_df)
#' inner_join(batting_df, uperson_df)
#'
#' # Left join: match, but preserve batting data
#' left_join(batting_df, uperson_df)
#'
#' # Anti join: find batters without person data
#' anti_join(batting_df, person_df)
#' # or people who didn't bat
#' anti_join(person_df, batting_df)
#' }
#' @name join.tbl_df
NULL

check_na_matches <- function(na_matches) {
  na_matches <- match.arg(na_matches, choices = c("na", "never"))
  accept_na_match <- (na_matches == "na")
  accept_na_match
}

#' @export
#' @rdname join.tbl_df
inner_join.tbl_df <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...,
                              na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  check_valid_names(tbl_vars(x))
  check_valid_names(tbl_vars(y))
  by <- common_by(by, x, y)
  suffix <- check_suffix(suffix)
  na_matches <- check_na_matches(na_matches)

  y <- auto_copy(x, y, copy = copy)

  vars <- join_vars(tbl_vars(x), tbl_vars(y), by, suffix)
  by_x <- vars$idx$x$by
  by_y <- vars$idx$y$by
  aux_x <- vars$idx$x$aux
  aux_y <- vars$idx$y$aux

  out <- inner_join_impl(x, y, by_x, by_y, aux_x, aux_y, na_matches, environment())
  names(out) <- vars$alias

  reconstruct_join(out, x, vars)
}

#' @export
#' @rdname join.tbl_df
nest_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ...) {
  name_var <- name %||% expr_name(enexpr(y))
  check_valid_names(tbl_vars(x))
  check_valid_names(tbl_vars(y))
  by <- common_by(by, x, y)

  y <- auto_copy(x, y, copy = copy)

  vars <- join_vars(tbl_vars(x), tbl_vars(y), by)
  by_x <- vars$idx$x$by
  by_y <- vars$idx$y$by
  aux_y <- vars$idx$y$aux
  if (keep) {
    aux_y <- c(by_y, aux_y)
  }

  out <- nest_join_impl(x, y, by_x, by_y, aux_y, name_var, environment())
  out
}


#' @export
#' @rdname join.tbl_df
left_join.tbl_df <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...,
                             na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  check_valid_names(tbl_vars(x))
  check_valid_names(tbl_vars(y))
  by <- common_by(by, x, y)
  suffix <- check_suffix(suffix)
  na_matches <- check_na_matches(na_matches)

  y <- auto_copy(x, y, copy = copy)

  vars <- join_vars(tbl_vars(x), tbl_vars(y), by, suffix)
  by_x <- vars$idx$x$by
  by_y <- vars$idx$y$by
  aux_x <- vars$idx$x$aux
  aux_y <- vars$idx$y$aux

  out <- left_join_impl(x, y, by_x, by_y, aux_x, aux_y, na_matches, environment())
  names(out) <- vars$alias

  reconstruct_join(out, x, vars)
}

#' @export
#' @rdname join.tbl_df
right_join.tbl_df <- function(x, y, by = NULL, copy = FALSE,
                              suffix = c(".x", ".y"), ...,
                              na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  check_valid_names(tbl_vars(x))
  check_valid_names(tbl_vars(y))
  by <- common_by(by, x, y)
  suffix <- check_suffix(suffix)
  na_matches <- check_na_matches(na_matches)

  y <- auto_copy(x, y, copy = copy)

  vars <- join_vars(tbl_vars(x), tbl_vars(y), by, suffix)
  by_x <- vars$idx$x$by
  by_y <- vars$idx$y$by
  aux_x <- vars$idx$x$aux
  aux_y <- vars$idx$y$aux

  out <- right_join_impl(x, y, by_x, by_y, aux_x, aux_y, na_matches, environment())
  names(out) <- vars$alias

  reconstruct_join(out, x, vars)
}

#' @export
#' @rdname join.tbl_df
full_join.tbl_df <- function(x, y, by = NULL, copy = FALSE,
                             suffix = c(".x", ".y"), ...,
                             na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  check_valid_names(tbl_vars(x))
  check_valid_names(tbl_vars(y))
  by <- common_by(by, x, y)
  suffix <- check_suffix(suffix)
  na_matches <- check_na_matches(na_matches)

  y <- auto_copy(x, y, copy = copy)

  vars <- join_vars(tbl_vars(x), tbl_vars(y), by, suffix)
  by_x <- vars$idx$x$by
  by_y <- vars$idx$y$by
  aux_x <- vars$idx$x$aux
  aux_y <- vars$idx$y$aux

  out <- full_join_impl(x, y, by_x, by_y, aux_x, aux_y, na_matches, environment())
  names(out) <- vars$alias

  reconstruct_join(out, x, vars)
}

#' @export
#' @rdname join.tbl_df
semi_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...,
                             na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  check_valid_names(tbl_vars(x), warn_only = TRUE)
  check_valid_names(tbl_vars(y), warn_only = TRUE)

  by <- common_by(by, x, y)
  y <- auto_copy(x, y, copy = copy)
  out <- semi_join_impl(x, y, by$x, by$y, check_na_matches(na_matches), environment())
  if (is_grouped_df(x)) {
    out <- grouped_df(out, group_vars(x), group_by_drop_default(x))
  }
  out
}

#' @export
#' @rdname join.tbl_df
anti_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...,
                             na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  check_valid_names(tbl_vars(x), warn_only = TRUE)
  check_valid_names(tbl_vars(y), warn_only = TRUE)

  by <- common_by(by, x, y)
  y <- auto_copy(x, y, copy = copy)
  out <- anti_join_impl(x, y, by$x, by$y, check_na_matches(na_matches), environment())
  if (is_grouped_df(x)) {
    out <- grouped_df(out, group_vars(x), group_by_drop_default(x))
  }
  out
}

reconstruct_join <- function(out, x, vars) {
  if (is_grouped_df(x)) {
    groups_in_old <- match(group_vars(x), tbl_vars(x))
    groups_in_alias <- match(groups_in_old, vars$x)
    out <- grouped_df(out, vars$alias[groups_in_alias], group_by_drop_default(x))
  }
  out
}


# Set operations ---------------------------------------------------------------

#' @export
# Can't use NextMethod() in R 3.1, r-lib/rlang#486
distinct.tbl_df <- distinct.data.frame
#' @export
# Can't use NextMethod() in R 3.1, r-lib/rlang#486
distinct_.tbl_df <- distinct_.data.frame
