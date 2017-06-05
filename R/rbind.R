list_or_dots <- function(...) {
  dots <- dots_list(...)
  if (!length(dots)) {
    return(dots)
  }

  # Old versions specified that first argument could be a list of
  # dataframeable objects
  if (is_list(dots[[1]])) {
    dots[[1]] <- map_if(dots[[1]], is_dataframe_like, as_tibble)
  }

  # Need to ensure that each component is a data frame or a vector
  # wrapped in a list:
  dots <- map_if(dots, is_dataframe_like, function(x) list(as_tibble(x)))
  dots <- map_if(dots, is_atomic, list)
  dots <- map_if(dots, is.data.frame, list)

  unlist(dots, recursive = FALSE)
}

is_dataframe_like <- function(x) {
  if (is_null(x))
    return(FALSE)

  # data frames are not data lists
  if (is.data.frame(x))
    return(FALSE)

  # Must be a list
  if (!is_list(x))
    return(FALSE)

  # 0 length named list (#1515)
  if (!is_null(names(x)) && length(x) == 0)
    return(TRUE)

  # With names
  if (!is_named(x))
    return(FALSE)

  # Where each element is an 1d vector or list
  if (!every(x, is_1d))
    return(FALSE)

  # All of which have the same length
  n <- map_int(x, length)
  if (any(n != n[1]))
    return(FALSE)

  TRUE
}


# Deprecated functions ----------------------------------------------------

#' @export
#' @rdname bind
#' @usage NULL
rbind_list <- function(...) {
  .Deprecated("bind_rows()")
  tbl_df(bind_rows_(list_or_dots(...), id = NULL))
}

#' @export
#' @rdname bind
#' @usage NULL
rbind_all <- function(x, id = NULL) {
  .Deprecated("bind_rows()")
  bind_rows_(x, id = id)
}
