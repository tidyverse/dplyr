
#' Apply a function to each group
#'
#' \Sexpr[results=rd, stage=render]{mypkg:::lifecycle("experimental")}
#'
#' @description
#'
#' `group_map()` and `group_walk()` are purrr-style functions that can
#' be used to iterate on grouped tibbles.
#'
#' @details
#'
#' - Use `group_map()` when `summarize()` is too limited, in terms of what you need
#'   to do and return for each group. `group_map()` is good for "data frame in, data frame out".
#'   If that is too limited, you need to use a [nested][group_nest()] or [split][group_split()] workflow.
#' - `group_map()` and `group_walk()` are an evolution of [do()], if you have used that before.
#'
#' Each conceptual group of the data frame is exposed to the function `.f` with two pieces of information:
#'
#'   - The subset of the data for the group, exposed as `.x`.
#'   - The key, a tibble with exactly one row and columns for each grouping variable, exposed as `.y`.
#'
#' `.f` must return a data frame that does not contain any of the grouping variables of `.tbl`.
#'
#' @family grouping functions
#'
#' @param .tbl A grouped tibble
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
#'
#' @return
#'  - `group_map()` row binds the data frames returned by `.f`
#'  - `group_walk()` calls `.f` for side effects and returns the input `.tbl`, invisibly
#'
#' @examples
#' mtcars %>%
#'   group_by(cyl) %>%
#'   group_map(~ head(.x, 2L))
#'
#' if (requireNamespace("broom", quietly = TRUE)) {
#'   iris %>%
#'     group_by(Species) %>%
#'     group_map(~ broom::tidy(lm(Petal.Length ~ Sepal.Length, data = .x)))
#' }
#'
#' iris %>%
#'   group_by(Species) %>%
#'   group_map(~ {
#'      quantile(.x$Petal.Length, probs = c(0.25, 0.5, 0.75)) %>%
#'      tibble::enframe(name = "prob", value = "quantile")
#'   })
#'
#' iris %>%
#'   group_by(Species) %>%
#'   group_map(~ {
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
#' @export
group_map <- function(.tbl, .f, ...) {
  UseMethod("group_map")
}

#' @export
group_map.formula <- function(.tbl, .f, ...) {
  abort("Did you forget to provide the primary input tibble?")
}

#' @export
group_map.function <- function(.tbl, .f, ...) {
  abort("Did you forget to provide the primary input tibble?")
}

#' @export
group_map.grouped_df <- function(.tbl, .f, ...) {
  .f <- rlang::as_function(.f)
  if (length(form <- formals(.f)) < 2 && ! "..." %in% names(form)){
    stop("The function must accept at least two arguments. You can use ... to absorb unused components")
  }

  # call the function on each group
  chunks <- group_split(.tbl, keep = FALSE)
  keys  <- group_keys(.tbl)
  group_keys <- map(seq_len(nrow(keys)), function(i) keys[i, , drop = FALSE])
  result_tibbles <- map2(chunks, group_keys, function(.x, .y){
    res <- .f(.x, .y, ...)
    if (!inherits(res, "data.frame")) {
      abort("The result of .f should be a data frame")
    }
    if (any(bad <- names(res) %in% group_vars(.tbl))) {
      abort(sprintf(
        "The returned data frame cannot contain the original grouping variables : ",
        paste(names(res)[bad], collapse = ", ")
      ))
    }
    bind_cols(.y[rep(1L, nrow(res)), , drop = FALSE], res)
  })

  # recalculates .rows based on the number of rows on each tibble
  .rows <- vector(mode = "list", length = length(result_tibbles))
  k <- 1L
  for (i in seq_along(result_tibbles)) {
    n <- nrow(result_tibbles[[i]])
    .rows[[i]] <- seq2(k, k + n - 1L)
    k <- k + n
  }

  # structure the result as a grouped data frame
  new_grouped_df(
    bind_rows(!!!result_tibbles),
    groups = tibble::add_column(keys, ".rows" := .rows)
  )
}

#' @export
#' @rdname group_map
group_walk <- function(.tbl, .f, ...) {
  UseMethod("group_walk")
}

#' @export
group_walk.grouped_df <- function(.tbl, .f, ...) {
  .f <- rlang::as_function(.f)

  # call the function on each group
  chunks <- group_split(.tbl, keep = FALSE)
  keys  <- group_keys(.tbl)
  group_keys <- map(seq_len(nrow(keys)), function(i) keys[i, , drop = FALSE])
  walk2(chunks, group_keys, .f)
  invisible(.tbl)
}
