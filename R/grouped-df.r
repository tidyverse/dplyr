utils::globalVariables(c("old_keys", "old_rows", ".rows", "new_indices", "new_rows", "new_rows_sizes", "needs_recycle"))

vec_split_id_order <- function(x) {
  split_id <- vec_group_pos(x)
  vec_slice(split_id, vec_order(split_id$key))
}

expand_groups <- function(old_groups, positions, nr) {
  .Call(`dplyr_expand_groups`, old_groups, positions, nr)
}

make_grouped_df_groups_attribute <- function(data, vars, drop = FALSE) {
  data <- as_tibble(data)

  is_symbol_list <- (is.list(vars) && all(sapply(vars, is.name)))
  if(!is_symbol_list && !is.character(vars)) {
    abort("incompatible `vars`, should be a list of symbols or a character vector")
  }
  if (is.list(vars)) {
    vars <- deparse_names(vars)
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
  c(old_keys, old_rows) %<-% vec_split_id_order(grouping_variables)

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

  structure(groups, .drop = drop)
}

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
  if (!length(vars)) {
    return(as_tibble(data))
  }

  # structure the grouped data
  new_grouped_df(
    data,
    groups = make_grouped_df_groups_attribute(data, vars, drop = drop)
  )
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
#' @param check_bounds whether to check all indices for out of bounds problems in grouped_df objects
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
validate_grouped_df <- function(x, check_bounds = FALSE) {
  result <- .Call(`dplyr_validate_grouped_df`, x, nrow(x), check_bounds)
  if (!is.null(result)) {
    abort(result, class = "dplyr_grouped_df_corrupt")
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
  lengths(group_rows(x))
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
  ungroup(x)
}

#' @export
ungroup.grouped_df <- function(x, ...) {
  attr(x, "groups") <- NULL
  attr(x, "class") <- c("tbl_df", "tbl", "data.frame")
  x
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

select_impl <- function(.data, vars) {
  positions <- match(vars, names(.data))
  if (any(test <- is.na(positions))) {
    wrong <- which(test)[1L]
    abort(
      glue(
        "invalid column index : {wrong} for variable: '{new}' = '{old}'",
        new = names(vars)[wrong], vars[wrong]
      ),
      .subclass = "dplyr_select_wrong_selection"
    )
  }

  out <- set_names(.data[, positions, drop = FALSE], names(vars))

  if (is_grouped_df(.data)) {
    # we might have to alter the names of the groups metadata
    groups <- attr(.data, "groups")

    # check grouped metadata
    group_names <- names(groups)[seq_len(ncol(groups) - 1L)]
    if (any(test <- ! group_names %in% vars)) {
      abort(
        glue("{col} not found in groups metadata. Probably a corrupt grouped_df object.", col = group_names[test[1L]]),
        "dplyr_select_corrupt_grouped_df"
      )
    }

    group_vars <- c(vars[vars %in% names(groups)], .rows = ".rows")
    groups <- select_impl(groups, group_vars)

    out <- new_grouped_df(out, groups)
  }

  out
}



#' @export
select.grouped_df <- function(.data, ...) {
  vars <- tidyselect::vars_select(tbl_vars(.data), !!!enquos(...))
  vars <- ensure_group_vars(vars, .data, notify = TRUE)
  select_impl(.data, vars)
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

# Set operations ---------------------------------------------------------------

#' @export
distinct.grouped_df <- function(.data, ..., .keep_all = FALSE) {
  dist <- distinct_prepare(
    .data,
    vars = enquos(...),
    group_vars = group_vars(.data),
    .keep_all = .keep_all
  )
  grouped_df(
    vec_slice(
      .data[, dist$keep, drop = FALSE],
      vec_unique_loc(.data[, dist$vars, drop = FALSE])
    ),
    groups(.data),
    group_by_drop_default(.data)
  )
}
