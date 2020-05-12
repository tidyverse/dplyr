#' A grouped data frame.
#'
#' @description
#' The easiest way to create a grouped data frame is to call the `group_by()`
#' method on a data frame or tbl: this will take care of capturing
#' the unevaluated expressions for you.
#'
#' These functions are designed for programmatic use. For data analysis
#' purposes see [group_data()] for the accessor functions that retrieve
#' various metadata from a grouped data frames.
#'
#' @keywords internal
#' @param data a tbl or data frame.
#' @param vars A character vector.
#' @param drop When `.drop = TRUE`, empty groups are dropped.
#'
#' @import vctrs
#' @export
grouped_df <- function(data, vars, drop = group_by_drop_default(data)) {
  if (!is.data.frame(data)) {
    abort("`data` must be a data frame.")
  }
  if (!is.character(vars)) {
    abort("`vars` must be a character vector.")
  }

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
    abort(glue("`vars` missing from `data`: {vars}."))
  }

  # Only train the dictionary based on selected columns
  group_vars <- as_tibble(data)[vars]
  split_key_loc <- vec_split_id_order(group_vars)
  old_keys <- split_key_loc$key
  old_rows <- split_key_loc$loc

  signal("", class = "dplyr_regroup")

  groups <- tibble(!!!old_keys, ".rows" := old_rows)

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
    expanded <- expand_groups(groups, positions, vec_size(old_keys))
    new_indices <- expanded$indices
    new_rows <- expanded$rows

    # Make the new keys from the old keys and the new_indices
    new_keys <- pmap(list(old_keys, new_indices, uniques), function(key, index, unique) {
      if(is.factor(key)) {
        new_factor(index, levels = levels(key))
      } else {
        vec_slice(unique, index)
      }
    })
    names(new_keys) <- vars

    groups <- tibble(!!!new_keys, ".rows" := new_rows)
  }

  structure(groups, .drop = drop)
}

count_regroups <- function(code) {
  i <- 0
  withCallingHandlers(code, dplyr_regroup = function(cnd) {
    i <<- i + 1
  })
  i
}

show_regroups <- function(code) {
  withCallingHandlers(code, dplyr_regroup = function(cnd) {
    cat("Regrouping...\n")
  })
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
      "`new_grouped_df()` incompatible argument.",
      x = "`x` is not a data frame.")
    )
  }
  if (!is.data.frame(groups) || tail(names(groups), 1L) != ".rows") {
    abort(c(
      "`new_grouped_df()` incompatible argument.",
      i = "`groups` should be a data frame, and its last column be called `.rows`."
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
  if (is.null(attr(x, "groups")) && !is.null(attr(x, "vars"))) {
    abort(c(
      "Corrupt `grouped_df` using old (< 0.8.0) format.",
      i = "Strip off old grouping with `ungroup()`."
    ))
  }

  result <- .Call(`dplyr_validate_grouped_df`, x, check_bounds)
  if (!is.null(result)) {
    abort(result)
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
  new_data_frame(vec_data(x), n = nrow(x))
}

#' @export
as_tibble.grouped_df <- function(x, ...) {
  new_tibble(vec_data(x), nrow = nrow(x))
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
    groups <- group_intersect(x, out)
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
    grouped_df(out, group_intersect(x, out), group_by_drop_default(x))
  } else {
    out
  }
}

#' @export
`[<-.grouped_df` <- function(x, i, j, ..., value) {
  out <- NextMethod()
  grouped_df(out, group_intersect(x, out), group_by_drop_default(x))
}

#' @export
`[[<-.grouped_df` <- function(x, ..., value) {
  out <- NextMethod()
  grouped_df(out, group_intersect(x, out), group_by_drop_default(x))
}

#' @export
`names<-.grouped_df` <- function(x, value) {
  data <- as.data.frame(x)
  names(data) <- value

  groups <- group_data(x)
  group_loc <- match(intersect(names(groups), names(x)), names(x))
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
  split_id <- vec_group_loc(x)
  split_id$loc <- new_list_of(split_id$loc, ptype = integer())
  vec_slice(split_id, vec_order(split_id$key))
}

group_intersect <- function(x, new) {
  intersect(group_vars(x), names(new))
}

