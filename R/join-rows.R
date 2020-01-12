join_rows <- function(x_key, y_key, type = c("inner", "left", "right", "outer")) {
  type <- arg_match(type)

  y_split <- vec_group_pos(y_key)
  matches <- vec_match(x_key, y_split$key)


  # expand indices
  x_loc <- seq_len(vec_size(x_key))
  y_loc <- y_split$pos[matches]
  if (type == "left") {
    y_loc <- map(y_loc, function(x) if (is.null(x)) NA_integer_ else x)
  }

  x_loc <- rep(x_loc, lengths(y_loc))
  y_loc <- vec_c(!!!y_loc, .ptype = integer())

  list(x = x_loc, y = y_loc)

}
