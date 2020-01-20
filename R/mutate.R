#' Create, modify, and delete columns
#'
#' `mutate()` adds new variables and preserves existing ones;
#' `transmute()` adds new variables and drops existing ones.
#' New variables overwrite existing variables of the same name.
#' Variables can be removed by setting their value to `NULL`.
#'
#' @section Useful mutate functions:
#'
#' * [`+`], [`-`], [log()], etc., for their usual mathematical meanings
#'
#' * [lead()], [lag()]
#'
#' * [dense_rank()], [min_rank()], [percent_rank()], [row_number()],
#'   [cume_dist()], [ntile()]
#'
#' * [cumsum()], [cummean()], [cummin()], [cummax()], [cumany()], [cumall()]
#'
#' * [na_if()], [coalesce()]
#'
#' * [if_else()], [recode()], [case_when()]
#'
#' @section Grouped tibbles:
#'
#' Because mutating expressions are computed within groups, they may
#' yield different results on grouped tibbles. This will be the case
#' as soon as an aggregating, lagging, or ranking function is
#' involved. Compare this ungrouped mutate:
#'
#' ```
#' starwars %>%
#'   mutate(mass / mean(mass, na.rm = TRUE)) %>%
#'   pull()
#' ```
#'
#' With the grouped equivalent:
#'
#' ```
#' starwars %>%
#'   group_by(gender) %>%
#'   mutate(mass / mean(mass, na.rm = TRUE)) %>%
#'   pull()
#' ```
#'
#' The former normalises `mass` by the global average whereas the
#' latter normalises by the averages within gender levels.
#'
#' @export
#' @inheritParams arrange
#' @param ... <[`tidy-eval`][dplyr_tidy_eval]> Name-value pairs.
#'   The name gives the name of the column in the output.
#'
#'   The value can be:
#'
#'   * A vector of length 1, which will be recycled to the correct length.
#'   * A vector the same length as the current group (or the whole data frame
#'     if ungrouped).
#'   * `NULL`, to remove the column.
#'   * A data frame or tibble, to create multiple columns in the output.
#'
#' @family single table verbs
#' @return
#' An object of the same type as `.data`.
#'
#' For `mutate()`:
#'
#' * Rows are not affected.
#' * Existing columns will be preserved unless explicitly modified.
#' * New columns will be added to the right of existing columns.
#' * Columns given value `NULL` will be removed
#' * Groups will be recomputed if a grouping variable is mutated.
#' * Data frame attributes are preserved.
#'
#' For `transmute()`:
#'
#' * Rows are not affected.
#' * Apart from grouping variables, existing columns will be remove unless
#'   explicitly kept.
#' * Column order matches order of expressions.
#' * Groups will be recomputed if a grouping variable is mutated.
#' * Data frame attributes are preserved.
#' @section Methods:
#' These function are **generic**s, which means that packages can provide
#' implementations (methods) for other classes. See the documentation of
#' individual methods for extra arguments and differences in behaviour.
#'
#' Methods available in currently loaded packages:
#'
#' * `mutate()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("mutate")}.
#' * `transmute()`: \Sexpr[stage=render,results=rd]{dplyr:::methods_rd("transmute")}.
#' @examples
#' # Newly created variables are available immediately
#' mtcars %>% as_tibble() %>% mutate(
#'   cyl2 = cyl * 2,
#'   cyl4 = cyl2 * 2
#' )
#'
#' # As well as adding new variables, you can use mutate() to
#' # remove variables and modify existing variables.
#' mtcars %>% as_tibble() %>% mutate(
#'   mpg = NULL,
#'   disp = disp * 0.0163871 # convert to litres
#' )
#'
#' # window functions are useful for grouped mutates
#' mtcars %>%
#'  group_by(cyl) %>%
#'  mutate(rank = min_rank(desc(mpg)))
#' # see `vignette("window-functions")` for more details
#'
#' # mutate() vs transmute --------------------------
#' # mutate() keeps all existing variables
#' mtcars %>%
#'   mutate(displ_l = disp / 61.0237)
#'
#' # transmute keeps only the variables you create
#' mtcars %>%
#'   transmute(displ_l = disp / 61.0237)
#'
#' # Experimental .remove = "used" allows you to remove variables
#' # that are used "up" when computing new variables:
#' mtcars %>%
#'   mutate(displ_l = disp / 61.0237, .remove = "used")
#'
#' # Grouping ----------------------------------------
#' # The mutate operation may yield different results on grouped
#' # tibbles because the expressions are computed within groups.
#' # The following normalises `mass` by the global average:
#' starwars %>%
#'   mutate(mass / mean(mass, na.rm = TRUE)) %>%
#'   pull()
#'
#' # Whereas this normalises `mass` by the averages within gender
#' # levels:
#' starwars %>%
#'   group_by(gender) %>%
#'   mutate(mass / mean(mass, na.rm = TRUE)) %>%
#'   pull()
#'
#' # Indirection ----------------------------------------
#' # Refer to column names stored as strings with the `.data` pronoun:
#' vars <- c("mass", "height")
#' mutate(starwars, prod = .data[[vars[[1]]]] * .data[[vars[[2]]]])
#' # Learn more in ?dplyr_tidy_eval
mutate <- function(.data, ...) {
  UseMethod("mutate")
}

#' @rdname mutate
#' @param .remove \Sexpr[results=rd]{lifecycle::badge("experimental")}
#'   This is an experimental argument that allows you to automatically remove
#'   columns from the output.
#'
#'   * `"none"`, the default, does not remove any variables.
#'   * `"used"` will remove any variables that have been used in the
#'     computation of new variables.
#'   * `"unused"` will remove all variables not used in the computation;
#'     it's useful for checking your work.
#'   * `"all"` will remove all existing variables, except for grouping keys
#'     (like [transmute()]).
#' @export
mutate.data.frame <- function(.data, ..., .remove = c("none", "used", "unused", "all")) {
  remove <- arg_match(.remove)

  cols <- mutate_cols(.data, ..., .track_usage = remove %in% c("used", "unused"))
  out <- dplyr_col_modify(.data, cols)

  if (remove == "none") {
    out
  } else if (remove == "used") {
    used <- names(.data)[attr(cols, "used")]
    keep <- setdiff(names(out), setdiff(used, names(cols)))
    out[keep]
  } else if (remove == "unused") {
    used <- names(.data)[attr(cols, "used")]
    keep <- union(used, names(cols))
    out[keep]
  } else if (remove == "all") {
    keep <- c(
      # ensure group vars present
      setdiff(group_vars(.data), names(cols)),
      # cols might contain NULLs
      intersect(names(cols), names(out))
    )
    out[keep]
  }
}

#' @rdname mutate
#' @export
transmute <- function(.data, ...) {
  UseMethod("transmute")
}

#' @export
transmute.data.frame <- function(.data, ...) {
  mutate(.data, ..., .remove = "all")
}

# Helpers -----------------------------------------------------------------

mutate_cols <- function(.data, ..., .track_usage = FALSE) {
  rows <- group_rows(.data)
  # workaround when there are 0 groups
  if (length(rows) == 0L) {
    rows <- list(integer(0))
  }
  rows_lengths <- .Call(`dplyr_vec_sizes`, rows)

  o_rows <- vec_order(vec_c(!!!rows, .ptype = integer()))
  mask <- DataMask$new(.data, caller_env(), rows, track_usage = .track_usage)

  dots <- enquos(...)
  dots_names <- names(dots)
  auto_named_dots <- names(enquos(..., .named = TRUE))
  if (length(dots) == 0L) {
    return(NULL)
  }

  new_columns <- list()

  tryCatch({
    for (i in seq_along(dots)) {
      # a list in which each element is the result of
      # evaluating the quosure in the "sliced data mask"
      # recycling it appropriately to match the group size
      #
      # TODO: reinject hybrid evaluation at the R level
      c(chunks, needs_recycle) %<-% mask$eval_all_mutate(dots[[i]])

      if (is.null(chunks)) {
        if (!is.null(dots_names) && dots_names[i] != "") {
          new_columns[[dots_names[i]]] <- zap()

          # we might get a warning if dots_names[i] does not exist
          suppressWarnings(mask$remove(dots_names[i]))
        }
        next
      }

      if (needs_recycle) {
        chunks <- map2(chunks, rows_lengths, function(chunk, n) {
          vec_recycle(chunk, n)
        })
      }
      result <- vec_slice(vec_c(!!!chunks), o_rows)

      not_named <- (is.null(dots_names) || dots_names[i] == "")
      if (not_named && is.data.frame(result)) {
        new_columns[names(result)] <- result

        # remember each result separately
        map2(seq_along(result), names(result), function(i, nm) {
          mask$add(nm, pluck(chunks, i))
        })
      } else {
        name <- if (not_named) auto_named_dots[i] else dots_names[i]

        # treat as a single output otherwise
        new_columns[[name]] <- result

        # remember
        mask$add(name, chunks)
      }

    }

  },
    rlang_error_data_pronoun_not_found = function(e) {
      stop_error_data_pronoun_not_found(conditionMessage(e), index = i, dots = dots, fn = "mutate")
    },
    vctrs_error_recycle_incompatible_size = function(e) {
      stop_mutate_recycle_incompatible_size(e, index = i, dots = dots)
    },
    dplyr_mutate_mixed_NULL = function(e) {
      stop_mutate_mixed_NULL(index = i, dots = dots)
    },
    dplyr_mutate_not_vector = function(e) {
      stop_mutate_not_vector(index = i, dots = dots, result = e$result)
    },
    vctrs_error_incompatible_type = function(e) {
      stop_combine(conditionMessage(e), index = i, dots = dots, fn = "mutate")
    },
    simpleError = function(e) {
      stop_eval_tidy(e, index = i, dots = dots, fn = "mutate")
    }
  )

  is_zap <- map_lgl(new_columns, inherits, "rlang_zap")
  new_columns[is_zap] <- rep(list(NULL), sum(is_zap))
  attr(new_columns, "used") <- mask$get_used()
  new_columns
}
