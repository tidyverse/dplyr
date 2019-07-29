#' A grouped data frame.
#'
#' The easiest way to create a grouped data frame is to call the `group_by()`
#' method on a data frame or tbl: this will take care of capturing
#' the unevaluated expressions for you.
#'
#' @keywords internal
#' @param data a tbl or data frame.
#' @param vars a character vector or a list of [name()]
#' @param drop When `.drop = TRUE`, empty groups are dropped.
#'
#' @import vctrs
#' @importFrom zeallot %<-%
#'
#' @export
grouped_df <- function(data, vars, drop = FALSE) {
  assert_that(
    is.data.frame(data),
    (is.list(vars) && all(sapply(vars, is.name))) || is.character(vars)
  )
  if (is.list(vars)) {
    vars <- deparse_names(vars)
  }
  data <- as_tibble(data)
  if (!length(vars)) {
    return(data)
  }

  unknown <- setdiff(vars, tbl_vars(data))
  if (n_unknown <- length(unknown)) {
    if(n_unknown == 1) {
      abort(glue("Column `{unknown}` is unknown"))
    } else {
      abort(glue("Column `{unknown}` are unknown", unknown = glue_collapse(unknown, sep  = ", ")))
    }
  }

  # Only train the dictionary based on selected columns
  grouping_variables <- select(ungroup(data), one_of(vars))
  c(old_indices, old_rows) %<-% vctrs:::vec_duplicate_split(grouping_variables)

  # Keys and associated rows, in order
  old_keys <- vec_slice(grouping_variables, old_indices)
  orders <- vec_order(old_keys)
  old_keys <- vec_slice(old_keys, orders)
  old_rows <- old_rows[orders]

  map2(old_keys, names(old_keys), function(x, n) {
    if (is.factor(x) && anyNA(x)) {
      warn(glue("Factor `{n}` contains implicit NA, consider using `forcats::fct_explicit_na`"))
    }
  })

  groups <- tibble(!!!old_keys, .rows := old_rows)

  if (!isTRUE(drop) && any(map_lgl(old_keys, is.factor))) {
    # Extra work is needed to auto expand empty groups

    uniques <- map(old_keys, function(.) {
      if (is.factor(.)) . else vec_unique(.)
    })

    # Internally we only work with integers
    #
    # so for any grouping column that is not a factor
    # we need to match the values to the unique values
    positions <- map2(old_keys, uniques, function(.x, .y) {
      if (is.factor(.x)) .x else vec_match(.x, .y)
    })

    # Expand groups internally adds empty groups recursively
    # we get back:
    # - indices: a list of how to vec_slice the current keys
    #            to get the new keys
    #
    # - rows:    the new list of rows (i.e. the same as old rows,
    #            but with some extra empty integer(0) added for empty groups)
    c(new_indices, new_rows) %<-% expand_groups(groups, positions, vec_size(old_keys))

    # Make the new keys from the old keys and the new_indices
    new_keys <- pmap(list(old_keys, new_indices, uniques), function(key, index, unique) {
      if(is.factor(key)) {
        new_factor(index, levels = levels(key))
      } else {
        vec_slice(unique, index)
      }
    })
    names(new_keys) <- names(grouping_variables)

    groups <- tibble(!!!new_keys, .rows := new_rows)
  }

  # structure the grouped data
  new_grouped_df(data, groups = structure(groups, .drop = drop))
}

#' Low-level construction and validation for the grouped_df class
#'
#' `new_grouped_df()` is a constructor designed to be high-performance so only
#' check types, not values. This means it is the caller's responsibility
#' to create valid values, and hence this is for expert use only.
#'
#' @param x A data frame
#' @param groups The grouped structure, `groups` should be a data frame.
#' Its last column should be called `.rows` and be
#' a list of 1 based integer vectors that all are between 1 and the number of rows of `.data`.
#' @param class additional class, will be prepended to canonical classes of a grouped data frame.
#' @param ... additional attributes
#'
#' @examples
#' # 5 bootstrap samples
#' tbl <- new_grouped_df(
#'   tibble(x = rnorm(10)),
#'   groups = tibble(".rows" := replicate(5, sample(1:10, replace = TRUE), simplify = FALSE))
#' )
#' # mean of each bootstrap sample
#' summarise(tbl, x = mean(x))
#'
#' @importFrom tibble new_tibble
#' @keywords internal
#' @export
new_grouped_df <- function(x, groups, ..., class = character()) {
  stopifnot(
    is.data.frame(x),
    is.data.frame(groups),
    tail(names(groups), 1L) == ".rows"
  )
  new_tibble(
    x,
    groups = groups,
    ...,
    nrow = NROW(x),
    class = c(class, "grouped_df")
  )
}

#' @description
#' `validate_grouped_df()` validates the attributes of a `grouped_df`.
#'
#' @rdname new_grouped_df
#' @export
validate_grouped_df <- function(x) {
  assert_that(is_grouped_df(x))

  groups <- attr(x, "groups")
  assert_that(
    is.data.frame(groups),
    ncol(groups) > 0,
    names(groups)[ncol(groups)] == ".rows",
    is.list(groups[[ncol(groups)]]),
    msg  = "The `groups` attribute is not a data frame with its last column called `.rows`"
  )

  n <- nrow(x)
  rows <- groups[[ncol(groups)]]
  for (i in seq_along(rows)) {
    indices <- rows[[i]]
    assert_that(
      is.integer(indices),
      msg = "`.rows` column is not a list of one-based integer vectors"
    )
    assert_that(
      all(indices >= 1 & indices <= n),
      msg = glue("indices of group {i} are out of bounds")
    )
  }

  x
}


setOldClass(c("grouped_df", "tbl_df", "tbl", "data.frame"))

#' @rdname grouped_df
#' @export
is.grouped_df <- function(x) inherits(x, "grouped_df")
#' @rdname grouped_df
#' @export
is_grouped_df <- is.grouped_df

group_sum <- function(x) {
  grps <- n_groups(x)
  paste0(commas(group_vars(x)), " [", big_mark(grps), "]")
}

#' @export
tbl_sum.grouped_df <- function(x) {
  c(
    NextMethod(),
    c("Groups" = group_sum(x))
  )
}

#' @export
group_size.grouped_df <- function(x) {
  group_size_grouped_cpp(x)
}

#' @export
n_groups.grouped_df <- function(x) {
  nrow(group_data(x))
}

#' @export
groups.grouped_df <- function(x) {
  syms(group_vars(x))
}

#' @export
group_vars.grouped_df <- function(x) {
  groups <- group_data(x)
  if (is.character(groups)) {
    # lazy grouped
    groups
  } else if (is.data.frame(groups)) {
    # resolved, extract from the names of the data frame
    head(names(groups), -1L)
  } else if (is.list(groups)) {
    # Need this for compatibility with existing packages that might
    # use the old list of symbols format
    map_chr(groups, as_string)
  }
}

#' @export
as.data.frame.grouped_df <- function(x, row.names = NULL,
                                     optional = FALSE, ...) {
  x <- ungroup(x)
  class(x) <- "data.frame"
  x
}

#' @export
as_tibble.grouped_df <- function(x, ...) {
  x <- ungroup(x)
  class(x) <- c("tbl_df", "tbl", "data.frame")
  x
}

#' @export
ungroup.grouped_df <- function(x, ...) {
  ungroup_grouped_df(x)
}

#' @importFrom tibble is_tibble
#' @export
`[.grouped_df` <- function(x, i, j, drop = FALSE) {
  y <- NextMethod()

  if (isTRUE(drop) && !is_tibble(y)) {
    return(y)
  }

  group_names <- group_vars(x)
  if (!all(group_names %in% names(y))) {
    tbl_df(y)
  } else {
    grouped_df(y, group_names, group_by_drop_default(x))
  }
}

#' @method rbind grouped_df
#' @export
rbind.grouped_df <- function(...) {
  bind_rows(...)
}

#' @method cbind grouped_df
#' @export
cbind.grouped_df <- function(...) {
  bind_cols(...)
}

#' Select grouping variables
#'
#' This selection helpers matches grouping variables. It can be used
#' in [select()] or [vars()][scoped] selections.
#'
#' @inheritParams tidyselect::select_helpers
#' @seealso [groups()] and [group_vars()] for retrieving the grouping
#'   variables outside selection contexts.
#'
#' @examples
#' gdf <- iris %>% group_by(Species)
#'
#' # Select the grouping variables:
#' gdf %>% select(group_cols())
#'
#' # Remove the grouping variables from mutate selections:
#' gdf %>% mutate_at(vars(-group_cols()), `/`, 100)
#' @export
group_cols <- function(vars = peek_vars()) {
  if (is_sel_vars(vars)) {
    matches <- match(vars %@% groups, vars)
    if (anyNA(matches)) {
      abort("Can't find the grouping variables")
    }
    matches
  } else {
    int()
  }
}

# One-table verbs --------------------------------------------------------------

# see arrange.r for arrange.grouped_df

.select_grouped_df <- function(.data, ..., notify = TRUE) {
  # Pass via splicing to avoid matching vars_select() arguments
  vars <- tidyselect::vars_select(tbl_vars(.data), !!!enquos(...))
  vars <- ensure_group_vars(vars, .data, notify = notify)
  select_impl(.data, vars)
}

#' @export
select.grouped_df <- function(.data, ...) {
  .select_grouped_df(.data, !!!enquos(...), notify = TRUE)
}
#' @export
select_.grouped_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  select.grouped_df(.data, !!!dots)
}

ensure_group_vars <- function(vars, data, notify = TRUE) {
  group_names <- group_vars(data)
  missing <- setdiff(group_names, vars)

  if (length(missing) > 0) {
    if (notify) {
      inform(glue(
        "Adding missing grouping variables: ",
        paste0("`", missing, "`", collapse = ", ")
      ))
    }
    vars <- c(set_names(missing, missing), vars)
  }

  vars
}

#' @export
rename.grouped_df <- function(.data, ...) {
  vars <- tidyselect::vars_rename(names(.data), ...)
  select_impl(.data, vars)
}
#' @export
rename_.grouped_df <- function(.data, ..., .dots = list()) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  rename(.data, !!!dots)
}


# Do ---------------------------------------------------------------------------

#' @export
do.grouped_df <- function(.data, ...) {
  index <- group_rows(.data)
  labels <- select(group_data(.data), -last_col())
  attr(labels, ".drop") <- NULL

  # Create ungroup version of data frame suitable for subsetting
  group_data <- ungroup(.data)

  args <- enquos(...)
  named <- named_args(args)
  mask <- new_data_mask(new_environment())

  n <- length(index)
  m <- length(args)

  # Special case for zero-group/zero-row input
  if (n == 0) {
    if (named) {
      out <- rep_len(list(list()), length(args))
      out <- set_names(out, names(args))
      out <- label_output_list(labels, out, groups(.data))
    } else {
      env_bind_do_pronouns(mask, group_data)
      out <- eval_tidy(args[[1]], mask)
      out <- out[0, , drop = FALSE]
      out <- label_output_dataframe(labels, list(list(out)), groups(.data), group_by_drop_default(.data))
    }
    return(out)
  }

  # Add pronouns with active bindings that resolve to the current
  # subset. `_i` is found in environment of this function because of
  # usual scoping rules.
  group_slice <- function(value) {
    if (missing(value)) {
      group_data[index[[`_i`]], , drop = FALSE]
    } else {
      group_data[index[[`_i`]], ] <<- value
    }
  }
  env_bind_do_pronouns(mask, group_slice)

  out <- replicate(m, vector("list", n), simplify = FALSE)
  names(out) <- names(args)
  p <- progress_estimated(n * m, min_time = 2)

  for (`_i` in seq_len(n)) {
    for (j in seq_len(m)) {
      out[[j]][`_i`] <- list(eval_tidy(args[[j]], mask))
      p$tick()$print()
    }
  }

  if (!named) {
    label_output_dataframe(labels, out, groups(.data), group_by_drop_default(.data))
  } else {
    label_output_list(labels, out, groups(.data))
  }
}
#' @export
do_.grouped_df <- function(.data, ..., env = caller_env(), .dots = list()) {
  dots <- compat_lazy_dots(.dots, env, ...)
  do(.data, !!!dots)
}

# Set operations ---------------------------------------------------------------

#' @export
distinct.grouped_df <- function(.data, ..., .keep_all = FALSE) {
  dist <- distinct_prepare(
    .data,
    vars = enquos(...),
    group_vars = group_vars(.data),
    .keep_all = .keep_all
  )
  vars <- match_vars(dist$vars, dist$data)
  keep <- match_vars(dist$keep, dist$data)
  out <- as_tibble(distinct_impl(dist$data, vars, keep, environment()))
  grouped_df(out, groups(.data), group_by_drop_default(.data))
}
#' @export
distinct_.grouped_df <- function(.data, ..., .dots = list(), .keep_all = FALSE) {
  dots <- compat_lazy_dots(.dots, caller_env(), ...)
  distinct(.data, !!!dots, .keep_all = .keep_all)
}
