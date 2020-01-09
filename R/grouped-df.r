#' A grouped data frame.
#'
#' The easiest way to create a grouped data frame is to call the `group_by()`
#' method on a data frame or tbl: this will take care of capturing
#' the unevaluated expressions for you.
#'
#' @keywords internal
#' @param data a tbl or data frame.
#' @param vars A character vector.
#' @param drop When `.drop = TRUE`, empty groups are dropped.
#'
#' @import vctrs
#' @importFrom zeallot %<-%
#'
#' @export
grouped_df <- function(data, vars, drop = FALSE) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(vars))

  if (length(vars) == 0) {
    as_tibble(data)
  } else {
    groups <- compute_groups(data, vars, drop = drop)
    new_grouped_df(data, groups)
  }
}

compute_groups <- function(data, vars, drop = FALSE) {
  unknown <- setdiff(vars, names(data))
  if (length(unknown) > 0) {
    vars <- paste0(encodeString(vars, quote = "`"), collapse = ", ")
    abort(glue("`vars` missing from `data`: {vars}"))
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
  if (!is.data.frame(x)) {
    abort(c(
      "`new_grouped_df()` incompatible argument",
      "`x` is not a data frame")
    )
  }
  if (!is.data.frame(groups) || tail(names(groups), 1L) != ".rows") {
    abort(c(
      "`new_grouped_df()` incompatible argument",
      "`groups` should be a data frame, and its last column be called `.rows`"
    ))
  }
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

#' @importFrom tibble is_tibble
#' @export
`[.grouped_df` <- function(x, i, j, drop = FALSE) {
  out <- NextMethod()

  if (!is.data.frame(out)) {
    return(out)
  }

  if (drop) {
    as_tibble(out)
  } else {
    groups <- intersect(names(out), group_vars(x))
    if ((missing(i) || nargs() == 2) && identical(groups, group_vars(x))) {
      new_grouped_df(out, group_data(x))
    } else {
      grouped_df(out, groups, group_by_drop_default(x))
    }
  }
}

#' @export
`$<-.grouped_df` <- function(x, name, ..., value) {
  out <- NextMethod()
  if (name %in% group_vars(x)) {
    grouped_df(out, intersect(names(out), group_vars(x)), group_by_drop_default(x))
  } else {
    out
  }
}

#' @export
`[<-.grouped_df` <- function(x, i, j, ..., value) {
  out <- NextMethod()
  grouped_df(out, intersect(names(out), group_vars(x)), group_by_drop_default(x))
}

#' @export
`[[<-.grouped_df` <- function(x, ..., value) {
  out <- NextMethod()
  grouped_df(out, intersect(names(out), group_vars(x)), group_by_drop_default(x))
}

#' @export
`names<-.grouped_df` <- function(x, value) {
  data <- as.data.frame(x)
  names(data) <- value

  groups <- group_data(x)
  group_loc <- match(intersect(names(x), names(groups)), names(groups))
  group_names <- c(value[group_loc], ".rows")
  if (!identical(group_names, names(groups))) {
    names(groups) <- c(value[group_loc], ".rows")
  }

  new_grouped_df(data, groups)
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

group_data_trim <- function(group_data, preserve = FALSE) {
  if (preserve) {
    return(group_data)
  }

  non_empty <- lengths(group_data$".rows") > 0
  group_data[non_empty, , drop = FALSE]
}

# Helpers -----------------------------------------------------------------

expand_groups <- function(old_groups, positions, nr) {
  .Call(`dplyr_expand_groups`, old_groups, positions, nr)
}

vec_split_id_order <- function(x) {
  split_id <- vec_group_pos(x)
  split_id$pos <- new_list_of(split_id$pos, ptype = integer())
  vec_slice(split_id, vec_order(split_id$key))
}

