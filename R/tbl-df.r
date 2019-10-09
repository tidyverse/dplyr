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

filter_update_rows <- function(n_rows, group_indices, keep, new_rows_sizes) {
  .Call(`dplyr_filter_update_rows`, n_rows, group_indices, keep, new_rows_sizes)
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
  rows <- group_rows(.data)

  # workaround when there are 0 groups
  if (length(rows) == 0L) {
    rows <- list(integer(0))
  }

  mask <- DataMask$new(.data, caller_env(), rows)
  on.exit(mask$restore())

  c(keep, new_rows_sizes, group_indices) %<-% mask$eval_all_filter(quo)

  out <- vec_slice(.data, keep)

  # regroup
  if (is_grouped_df(.data)) {
    new_groups <- group_data(.data)
    new_groups$.rows <- filter_update_rows(nrow(.data), group_indices, keep, new_rows_sizes)
    attr(out, "groups") <- new_groups

    if (!.preserve) {
      out <- regroup(out)
    }
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
slice.tbl_df <- function(.data, ..., .preserve = FALSE) {
  rows <- group_rows(.data)
  mask <- DataMask$new(.data, caller_env(), rows)

  dots <- enquos(...)
  if (is_empty(dots)) {
    return(.data)
  }

  rows <- group_rows(.data)
  mask <- DataMask$new(.data, caller_env(), rows)
  on.exit(mask$restore())

  quo <- quo(c(!!!dots))

  chunks <- mask$eval_all(quo)

  slice_indices <- new_list(length(rows))
  new_rows <- new_list(length(rows))
  k <- 1L

  for (group in seq_along(rows)) {
    current_rows <- rows[[group]]
    res <- chunks[[group]]

    if (is.logical(res) && all(is.na(res))) {
      res <- integer()
    } else if (is.numeric(res)) {
      res <- vec_cast(res, integer())
    } else if (!is.integer(res)) {
      abort(
        "slice() expressions should return indices (positive or negative integers)",
        "dplyr_slice_incompatible"
      )
    }

    if (length(res) == 0L) {
      # nothing to do
    } else if(all(res >= 0, na.rm = TRUE)) {
      res <- res[!is.na(res) & res <= length(current_rows) & res > 0]
    } else if (all(res <= 0, na.rm = TRUE)) {
      res <- setdiff(seq_along(current_rows), -res)
    } else {
      abort(
        "slice() expressions should return either all positive or all negative",
        "dplyr_slice_ambiguous"
      )
    }

    slice_indices[[group]] <- current_rows[res]
    new_k <- k + length(res)
    new_rows[[group]] <- seq2(k, new_k - 1L)
    k <- new_k
  }
  all_slice_indices <- vec_c(!!!slice_indices, .ptype = integer())

  out <- vec_slice(.data, all_slice_indices)

  if (is_grouped_df(.data)) {
    new_groups <- group_data(.data)
    new_groups$.rows <- new_list_of(new_rows, ptype = integer())
    attr(out, "groups") <- new_groups

    if (!.preserve) {
      out <- regroup(out)
    }
  }

  out
}

mutate_new_columns <- function(.data, ...) {
  rows <- group_rows(.data)
  # workaround when there are 0 groups
  if (length(rows) == 0L) {
    rows <- list(integer(0))
  }
  rows_lengths <- .Call(`dplyr_vec_sizes`, rows)

  o_rows <- vec_order(vec_c(!!!rows, .ptype = integer()))
  mask <- DataMask$new(.data, caller_env(), rows)

  dots <- enquos(...)
  dots_names <- names(dots)
  auto_named_dots <- names(enquos(..., .named = TRUE))
  if (length(dots) == 0L) {
    return(list())
  }

  dots <- enquos(...)
  dots_names <- names(dots)
  auto_named_dots <- names(enquos(..., .named = TRUE))
  if (length(dots) == 0L) {
    return(.data)
  }

  new_columns <- list()

  for (i in seq_along(dots)) {
    # a list in which each element is the result of
    # evaluating the quosure in the "sliced data mask"
    # recycling it appropriately to match the group size
    #
    # TODO: reinject hybrid evaluation at the R level
    c(chunks, needs_recycle) %<-% mask$eval_all_mutate(dots[[i]], dots_names, i)

    if (is.null(chunks)) {
      if (!is.null(dots_names) && dots_names[i] != "" && dots_names[[i]] %in% c(names(.data), names(new_columns))) {
        new_columns[[dots_names[i]]] <- zap()
        mask$remove(dots_names[i])
      }
      next
    }

    if (needs_recycle) {
      chunks <- map2(chunks, rows_lengths, function(chunk, n) {
        vec_recycle(chunk, n)
      })
    }
    result <- vec_slice(vec_c(!!!chunks), o_rows)

    if ((is.null(dots_names) || dots_names[i] == "") && is.data.frame(result)) {
      new_columns[names(result)] <- result

      # remember each result separately
      map2(seq_along(result), names(result), function(i, nm) {
        mask$add(nm, pluck(chunks, i))
      })
    } else {
      # treat as a single output otherwise
      new_columns[[ auto_named_dots[i] ]] <- result

      # remember
      mask$add(auto_named_dots[i], chunks)
    }

  }

  new_columns
}


#' @export
mutate.tbl_df <- function(.data, ...) {
  new_columns <- mutate_new_columns(.data, ...)
  if (!length(new_columns)) {
    return(.data)
  }

  out <- .data
  new_column_names <- names(new_columns)
  for (i in seq_along(new_columns)) {
    out[[new_column_names[i]]] <- if (!inherits(new_columns[[i]], "rlang_zap")) new_columns[[i]]
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
transmute.tbl_df <- function(.data, ...) {
  new_columns <- mutate_new_columns(.data, ...)

  out <- .data[, group_vars(.data), drop = FALSE]
  new_column_names <- names(new_columns)
  for (i in seq_along(new_columns)) {
    if (!inherits(new_columns[[i]], "rlang_zap")) {
      out[[new_column_names[i]]] <-  new_columns[[i]]
    }
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


DataMask <- R6Class("DataMask",
  public = list(
    initialize = function(data, caller, rows = group_rows(data)) {
      frame <- caller_env(n = 2)
      tidyselect::scoped_vars(tbl_vars(data), frame)

      private$old_group_size <- context_env[["..group_size"]]
      private$old_group_number <- context_env[["..group_number"]]

      private$rows <- rows

      private$data <- data
      private$caller <- caller
      private$bindings <- env()

      # A function that returns all the chunks for a column
      resolve_chunks <- if (inherits(data, "rowwise_df")) {
        function(index) {
          col <- .subset2(data, index)
          if (is_list(col) && !is.data.frame(col)) {
            map(rows, function(row) vec_slice(col, row)[[1L]])
          } else {
            map(rows, vec_slice, x = col)
          }
        }
      } else {
        function(index) map(rows, vec_slice, x = .subset2(data, index))
      }

      binding_fn <- function(index, chunks = resolve_chunks(index)){
        # chunks is a promise of the list of all chunks for the column
        # at this index, so resolve_chunks() is only called when
        # the active binding is touched
        function() .subset2(chunks, private$current_group)
      }
      env_bind_active(private$bindings, !!!set_names(map(seq_len(ncol(data)), binding_fn), names(data)))

      private$mask <- new_data_mask(private$bindings)
      private$mask$.data <- as_data_pronoun(private$mask)
    },

    add = function(name, chunks) {
      if (name %in% group_vars(private$data)) {
        abort(glue("Column `{name}` can't be modified because it's a grouping variable"))
      }
      env_bind_active(private$bindings, !!name := function() {
        .subset2(chunks, private$current_group)
      })
    },

    remove = function(name) {
      rm(list = name, envir = private$bindings)
    },

    eval_all = function(quo) {
      .Call(`dplyr_mask_eval_all`, quo, private, context_env)
    },

    eval_all_summarise = function(quo, dots_names, i) {
      .Call(`dplyr_mask_eval_all_summarise`, quo, private, context_env, dots_names, i)
    },

    eval_all_mutate = function(quo, dots_names, i) {
      .Call(`dplyr_mask_eval_all_mutate`, quo, private, context_env, dots_names, i)
    },

    eval_all_filter = function(quo) {
      .Call(`dplyr_mask_eval_all_filter`, quo, private, context_env, nrow(private$data))
    },

    restore = function() {
      context_env[["..group_size"]] <- private$old_group_size
      context_env[["..group_number"]] <- private$old_group_number
    }

  ),

  private = list(
    data = NULL,
    mask = NULL,
    old_group_size = 0L,
    old_group_number = 0L,
    old_vars = character(),
    rows = NULL,
    bindings = NULL,
    current_group = 0L,
    caller = NULL
  )
)

#' @importFrom tibble add_column
#' @export
summarise.tbl_df <- function(.data, ...) {
  rows <- group_rows(.data)
  # workaround when there are 0 groups
  if (length(rows) == 0L) {
    rows <- list(integer(0))
  }

  mask <- DataMask$new(.data, caller_env(), rows)
  on.exit(mask$restore())

  dots <- enquos(...)
  dots_names <- names(dots)
  auto_named_dots <- names(enquos(..., .named = TRUE))

  summaries <- list()

  .size <- 1L

  for (i in seq_along(dots)) {
    # a list in which each element is the result of
    # evaluating the quosure in the "sliced data mask"
    #
    # TODO: reinject hybrid evaluation at the R level
    quo <- dots[[i]]
    chunks <- mask$eval_all_summarise(quo, dots_names, i)

    # check that vec_size() of chunks is compatible with .size
    # and maybe update .size
    .size <- .Call(`dplyr_validate_summarise_sizes`, .size, chunks)

    result <- vec_c(!!!chunks)

    if ((is.null(dots_names) || dots_names[i] == "") && is.data.frame(result)) {
      summaries[names(result)] <- result

      # remember each result separately
      map2(seq_along(result), names(result), function(i, nm) {
        mask$add(nm, pluck(chunks, i))
      })
    } else {
      # treat as a single output otherwise
      summaries[[ auto_named_dots[i] ]] <-  result

      # remember
      mask$add(auto_named_dots[i], chunks)
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

check_valid_names <- function(names, warn_only = FALSE) {
  which_na <- which(is.na(names))
  alert <- if (warn_only) warn else abort

  if (length(which_na)) {
    alert(glue("Column `{cols}` cannot have NA as name",
      cols = glue_collapse(which_na, sep = ", ")
    ))
  }

  if (any(dup <- duplicated(names))){
    alert(glue("Column `{cols}` must have a unique name",
      cols = names[dup]
    ))
  }
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
  by_x <- check_by_x(vars$idx$x$by)

  by_y <- vars$idx$y$by
  aux_x <- vars$idx$x$aux
  aux_y <- vars$idx$y$aux
  by_names <- vars$alias[seq_len(length(by_y))]

  y_split <- vec_group_pos(set_names(y[, by_y, drop = FALSE], by_names))

  matches <- vec_match(
    set_names(x[, by_x, drop = FALSE], by_names),
    y_split$key
  )

  # expand indices
  x_indices <- seq_len(nrow(x))[!is.na(matches)]
  y_indices <- y_split$pos[matches[!is.na(matches)]]
  x_indices <- rep(x_indices, lengths(y_indices))
  y_indices <- vec_c(!!!y_indices, .pytype = integer())

  x_slice <- vec_slice(x, x_indices)
  y_slice <- vec_slice(y, y_indices)

  # joined columns, cast to their common types
  out <- new_list(ncol(x) + length(aux_y), names = vars$alias)

  # join columns, perhaps with casting,
  # x columns stay in same position
  join_ptype <- vec_ptype2(x[, by_x, drop = FALSE], set_names(y[, by_y, drop = FALSE], names(x)[by_x]))
  out[by_x] <- vec_cast(x_slice[, by_x, drop = FALSE], to = join_ptype)

  # other columns from x
  out[aux_x] <- x_slice[, aux_x, drop = FALSE]

  # then columns from y
  out[ncol(x) + seq_along(aux_y)] <- y_slice[, aux_y, drop = FALSE]

  reconstruct_join(as_tibble(out), x, vars)
}

#' @export
#' @rdname join.tbl_df
nest_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, keep = FALSE, name = NULL, ...) {
  name_var <- name %||% as_label(enexpr(y))

  check_valid_names(tbl_vars(x))
  check_valid_names(tbl_vars(y))
  by <- common_by(by, x, y)

  y <- auto_copy(x, y, copy = copy)

  vars <- join_vars(tbl_vars(x), tbl_vars(y), by)
  by_x <- check_by_x(vars$idx$x$by)
  by_names <- vars$alias[seq_len(length(by_x))]
  by_y <- vars$idx$y$by
  aux_y <- vars$idx$y$aux
  aux_x <- vars$idx$x$aux

  if (keep) {
    aux_y <- c(by_y, aux_y)
  }

  y_split <- vec_group_pos(set_names(y[, by_y, drop = FALSE], by_names))

  matches <- vec_match(
    set_names(x[, by_x, drop = FALSE], by_names),
    y_split$key
  )

  # expand indices
  y_indices <- y_split$pos

  # joined columns, cast to their common types
  joined <- x[, by_x, drop = FALSE]
  joined <- set_names(joined, vars$alias[seq_len(ncol(joined))])
  joined[] <- map2(joined, y[, by_y, drop = FALSE], function(joined_i, y_i) {
    vec_cast(joined_i, to = vec_ptype_common(joined_i, y_i))
  })

  # colums from x (no casting needed)
  x_result <- set_names(
    x[, aux_x, drop = FALSE],
    vars$alias[seq2(ncol(joined) + 1, ncol(x))]
  )

  # columns from y
  y_keep <- if (keep) y else y[, aux_y, drop = FALSE]
  y_result_list <- map(matches, function(idx) {
    if (identical(idx, NA_integer_)) {
      vec_slice(y_keep, 0L)
    } else {
      vec_slice(y_keep, y_indices[[idx]])
    }
  })

  out <- add_column(joined, !!!x_result, !!name_var := y_result_list)
  reconstruct_join(out, x, vars)
}

check_by_x <- function(by_x) {
  if (length(by_x) == 0L) {
    abort(
      "`by` must specify variables to join by",
      "dplyr_join_empty_by"
    )
  }
  by_x
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
  by_x <- check_by_x(vars$idx$x$by)

  by_y <- vars$idx$y$by
  aux_x <- vars$idx$x$aux
  aux_y <- vars$idx$y$aux

  # unique values and where they are in each
  y_split <- vec_group_pos(y[, by_y, drop = FALSE])

  # matching uniques in x with uniques in y
  matches <- vec_match(x[, by_x, drop = FALSE], set_names(y_split$key, names(x)[by_x]))

  # for each unique value in x, expand the ids according to the number
  # of matches in y
  x_indices <- vec_c(!!!map2(matches, seq_along(matches), function(match, ids, rhs_id) {
    if (is.na(match)) {
      ids
    } else {
      vec_repeat(ids, each = length(rhs_id[[match]]))
    }
  }, rhs_id = y_split$pos), .ptype = integer())
  x_slice <- vec_slice(x, x_indices)

  # same for ids of y
  y_indices <- vec_c(!!!map2(matches, seq_along(matches), function(match, ids, rhs_id) {
    if (is.na(match)) {
      NA_integer_
    } else {
      rhs_id[[match]]
    }
  }, rhs_id = y_split$pos), .ptype = integer())
  y_slice <- vec_slice(y, y_indices)

  out <- new_list(ncol(x) + length(aux_y), names = vars$alias)

  # join columns, perhaps with casting,
  # x columns stay in same position
  join_ptype <- vec_ptype2(x[, by_x, drop = FALSE], set_names(y[, by_y, drop = FALSE], names(x)[by_x]))
  out[by_x] <- vec_cast(x_slice[, by_x, drop = FALSE], to = join_ptype)

  # other columns from x
  out[aux_x] <- x_slice[, aux_x, drop = FALSE]

  # then columns from y
  out[ncol(x) + seq_along(aux_y)] <- y_slice[, aux_y, drop = FALSE]

  reconstruct_join(as_tibble(out), x, vars)
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
  by_x <- check_by_x(vars$idx$x$by)
  by_y <- vars$idx$y$by
  aux_x <- vars$idx$x$aux
  aux_y <- vars$idx$y$aux
  alias <- vars$alias

  # unique values and where they are in each
  x_split <- vec_group_pos(x[, by_x, drop = FALSE])

  # matching uniques in x with uniques in y
  matches <- vec_match(
    y[, by_y, drop = FALSE],
    set_names(x_split$key, names(y)[by_y])
  )

  # for each unique value in y, expand the ids according to the number
  # of matches in x
  y_indices <- vec_c(!!!map2(matches, seq_along(matches), function(match, id, lhs_id) {
    if (is.na(match)) {
      id
    } else {
      vec_repeat(id, each = length(lhs_id[[match]]))
    }
  }, lhs_id = x_split$pos), .ptype = integer())

  # same for ids of x
  x_indices <- vec_c(!!!map2(matches, seq_along(matches), function(match, id, lhs_id) {
    if (is.na(match)) {
      NA_integer_
    } else {
      vec_repeat(lhs_id[[match]], times = length(id))
    }
  }, lhs_id = x_split$pos), .ptype = integer())

  x_slice <- vec_slice(x, x_indices)
  y_slice <- vec_slice(y, y_indices)

  out <- new_list(ncol(x) + length(aux_y), names = vars$alias)

  # the joined columns (taken from `y`) and then cast to common type
  join_ptype <- vec_ptype2(x[, by_x, drop = FALSE], set_names(y[, by_y, drop = FALSE], names(x)[by_x]))
  out[by_x] <- vec_cast(set_names(y_slice[, by_y, drop = FALSE], names(x)[by_x]), to = join_ptype)

  # other colums from x
  out[aux_x] <- x_slice[, aux_x, drop = FALSE]

  # then columns from y
  out[ncol(x) + seq_along(aux_y)] <- y_slice[, aux_y, drop = FALSE]

  reconstruct_join(as_tibble(out), x, vars)
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
  by_x <- check_by_x(vars$idx$x$by)
  by_y <- vars$idx$y$by
  aux_x <- vars$idx$x$aux
  aux_y <- vars$idx$y$aux
  by_names <- vars$alias[seq_len(length(by_x))]

  # unique values and where they are in each
  x_split <- vec_group_pos(set_names(x[, by_x, drop = FALSE], by_names))
  y_split <- vec_group_pos(set_names(y[, by_y, drop = FALSE], by_names))

  # matching uniques in x with uniques in y and vice versa
  x_matches <- vec_match(x_split$key, y_split$key)
  y_matches <- vec_match(y_split$key, x_split$key)

  # expand x indices from x matches
  x_indices_one <- vec_c(
    !!!map2(x_matches, x_split$pos, function(match, ids, rhs_id) {
      if (is.na(match)) {
        ids
      } else {
        vec_repeat(ids, each = length(rhs_id[[match]]))
      }
    }, rhs_id = y_split$pos),
    .ptype = integer()
  )

  x_indices_two <- rep(NA_integer_,
    sum(lengths(y_split$pos[is.na(y_matches)]))
  )

  # rows in x
  y_indices_one <- vec_c(
    !!!map2(x_matches, x_split$pos, function(match, ids, rhs_id) {
      if (is.na(match)) {
        vec_repeat(NA_integer_, length(ids))
      } else {
        vec_repeat(rhs_id[[match]], times = length(ids))
      }
    }, rhs_id = y_split$pos),

    .ptype = integer()
  )

  # rows in y and not in x
  y_indices_two <- vec_c(!!!y_split$pos[is.na(y_matches)], .ptype = integer())

  out <- new_list(ncol(x) + length(aux_y), names = vars$alias)

  out[by_x] <- vec_rbind(
    vec_slice(x[, by_x, drop = FALSE], x_indices_one),
    vec_slice(y[, by_y, drop = FALSE], y_indices_two)
  )

  # other colums from x
  out[aux_x] <- vec_slice(x[, aux_x, drop = FALSE], c(x_indices_one, x_indices_two))

  # columns from y
  out[ncol(x) + seq_along(aux_y)] <- vec_slice(y[, aux_y, drop = FALSE], c(y_indices_one, y_indices_two))

  reconstruct_join(as_tibble(out), x, vars)
}

#' @export
#' @rdname join.tbl_df
semi_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...,
                             na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  check_valid_names(tbl_vars(x))
  check_valid_names(tbl_vars(y))

  by <- common_by(by, x, y)
  by_x <- check_by_x(by$x)
  y <- auto_copy(x, y, copy = copy)
  suffix <- check_suffix(c(".x", ".y"))
  na_matches <- check_na_matches(na_matches)

  y <- auto_copy(x, y, copy = copy)

  vars <- join_vars(tbl_vars(x), tbl_vars(y), by, suffix)
  by_x <- check_by_x(vars$idx$x$by)
  by_y <- vars$idx$y$by
  aux_x <- vars$idx$x$aux
  aux_y <- vars$idx$y$aux

  y_split <- vec_group_pos(
    set_names(y[, by_y, drop = FALSE], names(x)[by_x])
  )
  x_indices <- which(!is.na(vec_match(x[, by_x, drop = FALSE], y_split$key)))

  out <- new_list(ncol(x), names = names(x))
  out[by_x] <- vec_cast(
    vec_slice(x[, by_x, drop = FALSE], x_indices),
    vec_ptype2(x[, by_x, drop = FALSE], y[, by_y, drop = FALSE])
  )

  out[aux_x] <- vec_slice(x[, aux_x, drop = FALSE], x_indices)
  reconstruct_join(as_tibble(out), x, vars)
}

#' @export
#' @rdname join.tbl_df
anti_join.tbl_df <- function(x, y, by = NULL, copy = FALSE, ...,
                             na_matches = pkgconfig::get_config("dplyr::na_matches")) {
  check_valid_names(tbl_vars(x))
  check_valid_names(tbl_vars(y))

  by <- common_by(by, x, y)
  by_x <- check_by_x(by$x)
  y <- auto_copy(x, y, copy = copy)
  suffix <- check_suffix(c(".x", ".y"))
  na_matches <- check_na_matches(na_matches)

  y <- auto_copy(x, y, copy = copy)

  vars <- join_vars(tbl_vars(x), tbl_vars(y), by, suffix)
  by_x <- check_by_x(vars$idx$x$by)
  by_y <- vars$idx$y$by
  aux_x <- vars$idx$x$aux
  aux_y <- vars$idx$y$aux

  y_split <- vec_group_pos(
    set_names(y[, by_y, drop = FALSE], names(x)[by_x])
  )
  x_indices <- which(is.na(vec_match(x[, by_x, drop = FALSE], y_split$key)))

  out <- new_list(ncol(x), names = names(x))
  out[by_x] <- vec_cast(
    vec_slice(x[, by_x, drop = FALSE], x_indices),
    vec_ptype2(x[, by_x, drop = FALSE], y[, by_y, drop = FALSE])
  )

  out[aux_x] <- vec_slice(x[, aux_x, drop = FALSE], x_indices)
  reconstruct_join(as_tibble(out), x, vars)
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
