join_rows <- function(x_key, y_key, type = c("inner", "left", "right", "full"), na_equal = TRUE) {
  type <- arg_match(type)

  # Find matching rows in y for each row in x
  y_split <- vec_group_loc(y_key)
  matches <- vec_match(x_key, y_split$key, na_equal = na_equal)
  y_loc <- y_split$loc[matches]

  if (type == "left" || type == "full") {
    y_loc <- vec_assign(y_loc, vec_equal_na(matches), list(NA_integer_))
  }

  x_loc <- seq_len(vec_size(x_key))

  # flatten index list
  x_loc <- rep(x_loc, lengths(y_loc))
  y_loc <- vec_c(!!!y_loc, .ptype = integer())

  y_extra <- integer()

  if (type == "right" || type == "full") {
    miss_x <- !vec_in(y_key, x_key, na_equal)

    if (any(miss_x)) {
      y_extra <- seq_len(vec_size(y_key))[miss_x]
    }
  }

  list(x = x_loc, y = y_loc, y_extra = y_extra)
}
