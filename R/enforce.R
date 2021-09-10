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

enforce_show <- function(.data, ...) {
  dots <- enquos(..., .ignore_empty = "all")
  n_dots <- length(dots)
  seq_dots <- seq_len(n_dots)

  requirements <- names(exprs_auto_name(dots))
  requirements <- vec_as_names(requirements, repair = "unique", quiet = TRUE)

  # Ensure unique names in the `transmute()`
  dots <- set_names(dots, glue("dplyr:::enforce-{seq_dots}"))

  successes <- transmute(.data, !!!dots)
  successes <- unstructure(successes)

  if (length(successes) != n_dots) {
    # Can happen if a `NULL` expression drops the column
    abort("Each expression in `...` must evaluate to a logical vector.")
  }

  locations <- vec_init(list(), n_dots)

  for (i in seq_dots) {
    success <- successes[[i]]

    if (!is_logical(success)) {
      abort(glue(
        "Each expression in `...` must evaluate to a logical vector. ",
        "Expression {i} does not evaluate to a logical vector."
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
