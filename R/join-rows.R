join_rows <- function(x_key, y_key, type = c("inner", "left", "right", "full")) {
  type <- arg_match(type)
  if (type == "right") {
    out <- join_rows(y_key, x_key, type = "left")
    return(list(x = out$y, y = out$x))
  }

  # Find matching rows in y for each row in x
  y_split <- vec_group_pos(y_key)
  matches <- vec_match(x_key, y_split$key)
  y_loc <- y_split$pos[matches]

  if (type == "left" || type == "full") {
    y_loc <- map(y_loc, function(x) if (is.null(x)) NA_integer_ else x)
  }

  x_loc <- seq_len(vec_size(x_key))
  if (type == "full") {
    miss_x <- !vec_in(y_key, x_key)
    x_loc <- c(x_loc, rep(NA_integer_, sum(miss_x)))
    y_loc <- c(y_loc, as.list(seq_len(vec_size(y_key))[miss_x]))
  }

  # flatten index list
  x_loc <- rep(x_loc, lengths(y_loc))
  y_loc <- vec_c(!!!y_loc, .ptype = integer())

  list(x = x_loc, y = y_loc)
}
