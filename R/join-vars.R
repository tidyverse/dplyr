join_vars <- function(x_names, y_names, by, suffix = list(x = ".x", y = ".y")) {
  # Record position of join keys
  idx <- get_join_var_idx(x_names, y_names, by)

  x_names_by <- x_names[idx$x$by]
  x_names_aux <- x_names[idx$x$aux]
  y_names_aux <- y_names[idx$y$aux]

  # Add suffix where needed
  x_new <- x_names
  x_new[idx$x$aux] <- add_suffixes(x_names_aux, c(x_names_by, y_names_aux), suffix$x)
  y_new <- add_suffixes(y_names_aux, x_names, suffix$y)

  x_x <- seq_along(x_names)
  x_y <- idx$y$by[match(x_names, by$x)]
  y_x <- rep_along(idx$y$aux, NA)
  y_y <- seq_along(idx$y$aux)

  # Return a list with 3 parallel vectors
  # At each position, values in the 3 vectors represent
  #  alias - name of column in join result
  #  x - position of column from left table or NA if only from right table
  #  y - position of column from right table or NA if only from left table
  ret <- list(alias = c(x_new, y_new), x = c(x_x, y_x), y = c(x_y, y_y))
  # In addition, the idx component contains indices of "by" and "aux" variables
  # for x and y, respectively (see get_join_var_idx())
  ret$idx <- idx
  ret
}

get_join_var_idx <- function(x_names, y_names, by) {
  x_idx <- get_by_aux(x_names, by$x)
  y_idx <- get_by_aux(y_names, by$y)

  list(x = x_idx, y = y_idx)
}

get_by_aux <- function(names, by) {
  if (length(by) > 0) {
    by <- match(by, names)
    aux <- seq_along(names)[-by]
  } else {
    by <- integer()
    aux <- seq_along(names)
  }

  list(by = by, aux = aux)
}
