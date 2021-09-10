#' Enforce requirements
#'
#' @description
#' \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#' These functions represent an easy and lightweight way to check assumptions
#' about your data directly in a pipeline.
#'
#' - `enforce()` takes a set of expressions, evaluates them on the rows of
#'   `.data`, and checks that each expression returns `TRUE` for every row. If
#'   any rows evaluate to `FALSE`, an error is thrown. Otherwise, `.data` is
#'   returned invisibly.
#'
#' - `enforce_last()` returns more information about the last error thrown by
#'   `enforce()`. It returns a data frame containing the expression that
#'   failed, the row in `.data` that failed, and a slice of the original data
#'   containing the failing row.
#'
#' - `enforce_show()` is similar to `enforce()`, but returns the data frame
#'   typically retrieved by `enforce_last()` rather than erroring. It is
#'   typically only useful for debugging or for checking requirements
#'   interactively.
#'
#' @param .data A data frame.
#'
#' @param ... Expressions to evaluate on the rows of `.data`. Each expression
#'   must return a logical vector the same length as the number of rows in
#'   `.data`.
#'
#'   Optionally, expressions may be named. The names will be used to provide
#'   more informative error messages about the requirements that weren't met.
#'
#' @details
#' If `.data` is a grouped data frame, each expression in `...` is evaluated
#' on a per group basis. This is often useful for performing per group checks,
#' but if your check doesn't utilize the grouping structure, then it is often
#' much faster to call `enforce()` on ungrouped data.
#'
#' Missing values resulting from evaluating `...` are equivalent to `TRUE` and
#' will not result in a failure.
#'
#' @export
#' @examples
#' # `enforce()` throws an error if your requirements aren't met
#' try({
#'   starwars %>%
#'     enforce(
#'       "Height is within expected range" = between(height, 80, 220),
#'       birth_year > 10
#'     )
#' })
#'
#' # Retrieve information about the failing rows with `enforce_last()`
#' enforce_last()
#'
#' # TODO: Replace with better example? Function from dplyr?
#' # This is particularly useful before a join to validate your keys
#' key_detect_valid <- function(...) {
#'   key <- tibble::tibble(...)
#'   # Enforce no missing values and unique keys
#'   vctrs::vec_detect_complete(key) & !vctrs::vec_duplicate_detect(key)
#' }
#'
#' df1 <- tibble(x = c(1, 2, NA))
#' df2 <- tibble(x = c(1, 1, 2), y = c("first", "second", "third"))
#'
#' # Missing key
#' try(enforce(df1, "Keys are unique and not missing" = key_detect_valid(x)))
#' enforce_last()
#'
#' # Duplicate key
#' try(enforce(df2, "Keys are unique and not missing" = key_detect_valid(x)))
#' enforce_last()
#'
#' # Otherwise you might end up with multiple matches or missing values
#' # in your join, which are often unexpected
#' left_join(df1, df2, by = "x")
enforce <- function(.data, ...) {
  failures <- enforce_show(.data, ...)

  if (nrow(failures) == 0L) {
    return(invisible(.data))
  }

  enforce_last_set(failures)

  info <- vec_count(failures$.requirement, sort = "location")
  requirements <- info$key
  counts <- info$count

  plural <- if_else(counts == 1L, true = "", false = "s")

  bullets <- glue("{counts} row{plural} failed: {requirements}.")
  bullets <- set_names(bullets, vec_rep("x", times = length(bullets)))

  header <- "Enforcement failed. The following requirements were not met:"
  footer <- c(i = "Locate failures by calling `enforce_last()`.")

  abort(c(header, bullets, footer))
}

#' @export
#' @rdname enforce
enforce_show <- function(.data, ...) {
  dots <- enquos(..., .ignore_empty = "all")
  n_dots <- length(dots)
  seq_dots <- seq_len(n_dots)

  requirements <- names(exprs_auto_name(dots))
  requirements <- vec_as_names(requirements, repair = "unique", quiet = TRUE)

  # Ensure unique names in the `transmute()`
  dots <- set_names(dots, glue("dplyr:::enforce-{seq_dots}"))

  successes <- with_handlers(
    transmute(.data, !!!dots),
    "dplyr:::mutate_error" = enforce_rethrow_mutate_error
  )

  successes <- unstructure(successes)

  if (length(successes) != n_dots) {
    # Can happen if a `NULL` expression drops the column
    abort("Each expression in `...` must evaluate to a logical vector.")
  }

  locations <- vec_init(list(), n_dots)

  for (i in seq_dots) {
    success <- successes[[i]]

    if (!is_logical(success)) {
      abort(c(
        "Each expression in `...` must evaluate to a logical vector. ",
        x = glue("Expression {i} does not evaluate to a logical vector.")
      ))
    }

    failure <- !success

    if (any(failure, na.rm = TRUE)) {
      locations[[i]] <- which(failure)
    }
  }

  failures <- !vec_equal_na(locations)

  requirements <- requirements[failures]
  locations <- locations[failures]
  counts <- list_sizes(locations)

  requirements <- vec_rep_each(requirements, times = counts)
  rows <- vec_unchop(locations, ptype = integer())
  data <- dplyr_row_slice(.data, rows)

  tibble::add_column(
    data,
    .requirement = requirements,
    .row = rows,
    .before = 1L,
    .name_repair = "minimal"
  )
}

#' @export
#' @rdname enforce
enforce_last <- function() {
  out <- enforce_last_get()

  if (is_null(out)) {
    abort(paste0(
      "Can't show enforcement failure information ",
      "because no failures have been recorded yet."
    ))
  }

  out
}


dplyr_enforce_env <- new_environment()

enforce_last_set <- function(x) {
  dplyr_enforce_env$failures <- x
  invisible(x)
}
enforce_last_get <- function() {
  dplyr_enforce_env$failures
}

enforce_rethrow_mutate_error <- function(cnd) {
  i <- cnd$error_index

  message <- c(
    "`enforce()` failed at the implicit `transmute()` step.",
    i = glue("Expression {i} failed.")
  )

  abort(message, class = "dplyr_error", parent = cnd)
}
