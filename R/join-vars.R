join_vars <- function(x_names, y_names, by, suffix = list(x = ".x", y = ".y")) {
  # Remove join keys from y
  y_idx <- get_by_aux(y_names, by$y)
  y_names_aux <- y_names[y_idx$aux]

  # Record position of join keys in x
  x_idx <- get_by_aux(x_names, by$x)
  x_names_aux <- x_names[x_idx$aux]

  # Add suffix where needed
  x_new <- x_names
  x_new[x_idx$aux] <- add_suffixes(x_names_aux, y_names_aux, suffix$x)
  y_new <- add_suffixes(y_names_aux, x_names, suffix$y)

  x_x <- seq_along(x_names)
  x_y <- y_idx$by[match(x_names, by$x)]
  y_x <- rep_along(y_idx$aux, NA)
  y_y <- seq_along(y_idx$aux)

  # Return a list with 3 parallel vectors
  # At each position, values in the 3 vectors represent
  #  alias - name of column in join result
  #  x - position of column from left table or NA if only from right table
  #  y - position of column from right table or NA if only from left table
  ret <- list(alias = c(x_new, y_new), x = c(x_x, y_x), y = c(x_y, y_y))
  ret$joiner <- !(is.na(ret$x) | is.na(ret$y))
  ret
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

add_suffixes <- function(x, y, suffix) {
  if (identical(suffix, "")) {
    return(x)
  }

  out <- chr_along(x)
  for (i in seq_along(x)) {
    nm <- x[[i]]
    while (nm %in% y || nm %in% out) {
      nm <- paste0(nm, suffix)
    }

    out[[i]] <- nm
  }
  out
}
