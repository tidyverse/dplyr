
as_group_map_function <- function(.f) {
  .f <- rlang::as_function(.f)
  if (length(form <- formals(.f)) < 2 && ! "..." %in% names(form)){
    stop("The function must accept at least two arguments. You can use ... to absorb unused components")
  }
  .f
}

#' Apply a function to each group
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' `group_map()`, `group_modify()` and `group_walk()` are purrr-style functions that can
#' be used to iterate on grouped tibbles.
#'
#' @details
#' Use `group_modify()` when `summarize()` is too limited, in terms of what you need
#'   to do and return for each group. `group_modify()` is good for "data frame in, data frame out".
#'   If that is too limited, you need to use a [nested][group_nest()] or [split][group_split()] workflow.
#'   `group_modify()` is an evolution of [do()], if you have used that before.
#'
#' Each conceptual group of the data frame is exposed to the function `.f` with two pieces of information:
#'
#'   - The subset of the data for the group, exposed as `.x`.
#'   - The key, a tibble with exactly one row and columns for each grouping variable, exposed as `.y`.
#'
#' For completeness, `group_modify()`, `group_map` and `group_walk()` also work on
#' ungrouped data frames, in that case the function is applied to the
#' entire data frame (exposed as `.x`), and `.y` is a one row tibble with no
#' column, consistently with [group_keys()].
#'
#' @family grouping functions
#'
#' @param .data A grouped tibble
#' @param .f A function or formula to apply to each group. It must return a data frame.
#'
#'   If a __function__, it is used as is. It should have at least 2 formal arguments.
#'
#'   If a __formula__, e.g. `~ head(.x)`, it is converted to a function.
#'
#'   In the formula, you can use
#'
#'   -  `.` or `.x` to refer to the subset of rows of `.tbl`
#'   for the given group
#'
#'   - `.y` to refer to the key, a one row tibble with one column per grouping variable
#'   that identifies the group
#'
#' @param ... Additional arguments passed on to `.f`
#' @param .keep are the grouping variables kept in `.x`
#'
#' @return
#'  - `group_modify()` returns a grouped tibble. In that case `.f` must return a data frame.
#'  - `group_map()` returns a list of results from calling `.f` on each group
#'  - `group_walk()` calls `.f` for side effects and returns the input `.tbl`, invisibly
#'
#' @examples
#'
#' # return a list
#' mtcars %>%
#'   group_by(cyl) %>%
#'   group_map(~ head(.x, 2L))
#'
#' # return a tibble grouped by `cyl` with 2 rows per group
#' # the grouping data is recalculated
#' mtcars %>%
#'   group_by(cyl) %>%
#'   group_modify(~ head(.x, 2L))
#'
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   # a list of tibbles
#'   iris %>%
#'     group_by(Species) %>%
#'     group_map(~ broom::tidy(lm(Petal.Length ~ Sepal.Length, data = .x)))
#'
#'   # a restructured grouped tibble
#'   iris %>%
#'     group_by(Species) %>%
#'     group_modify(~ broom::tidy(lm(Petal.Length ~ Sepal.Length, data = .x)))
#' }
#'
#' # a list of vectors
#' iris %>%
#'   group_by(Species) %>%
#'   group_map(~ quantile(.x$Petal.Length, probs = c(0.25, 0.5, 0.75)))
#'
#' # to use group_modify() the lambda must return a data frame
#' iris %>%
#'   group_by(Species) %>%
#'   group_modify(~ {
#'      quantile(.x$Petal.Length, probs = c(0.25, 0.5, 0.75)) %>%
#'      tibble::enframe(name = "prob", value = "quantile")
#'   })
#'
#' iris %>%
#'   group_by(Species) %>%
#'   group_modify(~ {
#'     .x %>%
#'       purrr::map_dfc(fivenum) %>%
#'       mutate(nms = c("min", "Q1", "median", "Q3", "max"))
#'   })
#'
#' # group_walk() is for side effects
#' dir.create(temp <- tempfile())
#' iris %>%
#'   group_by(Species) %>%
#'   group_walk(~ write.csv(.x, file = file.path(temp, paste0(.y$Species, ".csv"))))
#' list.files(temp, pattern = "csv$")
#' unlink(temp, recursive = TRUE)
#'
#' # group_modify() and ungrouped data frames
#' mtcars %>%
#'   group_modify(~ head(.x, 2L))
#'
#' @export
group_map <- function(.data, .f, ..., .keep = FALSE) {
  UseMethod("group_map")
}

#' @export
group_map.data.frame <- function(.data, .f, ..., .keep = FALSE) {
  .f <- as_group_map_function(.f)

  # call the function on each group
  chunks <- if (is_grouped_df(.data)) {
    group_split(.data, .keep = isTRUE(.keep))
  } else {
    group_split(.data)
  }
  keys  <- group_keys(.data)
  group_keys <- map(seq_len(nrow(keys)), function(i) keys[i, , drop = FALSE])

  if (length(chunks)) {
    map2(chunks, group_keys, .f, ...)
  } else {
    # calling .f with .x and .y set to prototypes
    structure(list(), ptype = .f(attr(chunks, "ptype"), keys[integer(0L), ], ...))
  }
}

#' @rdname group_map
#' @export
group_modify <- function(.data, .f, ..., .keep = FALSE) {
  UseMethod("group_modify")
}

#' @export
group_modify.data.frame <- function(.data, .f, ..., .keep = FALSE) {
  .f <- as_group_map_function(.f)
  .f(.data, group_keys(.data), ...)
}

#' @export
group_modify.grouped_df <- function(.data, .f, ..., .keep = FALSE) {
  tbl_group_vars <- group_vars(.data)
  .f <- as_group_map_function(.f)
  fun <- function(.x, .y){
    res <- .f(.x, .y, ...)
    if (!inherits(res, "data.frame")) {
      abort("The result of .f should be a data frame")
    }
    if (any(bad <- names(res) %in% tbl_group_vars)) {
      abort(paste0(
        "The returned data frame cannot contain the original grouping variables: ",
        paste(names(res)[bad], collapse = ", ")
      ))
    }
    bind_cols(.y[rep(1L, nrow(res)), , drop = FALSE], res)
  }
  chunks <- group_map(.data, fun, .keep = .keep)
  res <- if (length(chunks) > 0L) {
    bind_rows(!!!chunks)
  } else {
    attr(chunks, "ptype")
  }
  grouped_df(res, group_vars(.data), group_by_drop_default(.data))
}

#' @export
#' @rdname group_map
group_walk <- function(.data, .f, ...) {
  group_map(.data, .f, ...)
  invisible(.data)
}
