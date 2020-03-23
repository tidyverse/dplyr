join_rows <- function(x_key, y_key, type = c("inner", "left", "right", "full"), na_equal = TRUE) {
  type <- arg_match(type)

  # Find matching rows in y for each row in x
  y_split <- vec_group_loc(y_key)
  tryCatch(
    matches <- vec_match(x_key, y_split$key, na_equal = na_equal),
    vctrs_error_incompatible_type = function(cnd) {
      x_name <- sub("^needles[$]", "", cnd$x_arg)
      y_name <- sub("^haystack[$]", "", cnd$y_arg)

      abort(c(
        glue("Can't join on `x${x_name}` x `y${y_name}` because of incompatible types. "),
        i = glue("`x${x_name}` is of type <{x_type}>>.", x_type = vec_ptype_full(cnd$x)),
        i = glue("`y${y_name}` is of type <{y_type}>>.", y_type = vec_ptype_full(cnd$y))
      ))
    }
  )

  y_loc <- y_split$loc[matches]

  if (type == "left" || type == "full") {
    if (anyNA(matches)) {
      y_loc <- vec_assign(y_loc, vec_equal_na(matches), list(NA_integer_))
    }
  }

  x_loc <- seq_len(vec_size(x_key))

  # flatten index list
  x_loc <- rep(x_loc, lengths(y_loc))
  y_loc <- index_flatten(y_loc)

  y_extra <- integer()

  if (type == "right" || type == "full") {
    miss_x <- !vec_in(y_key, x_key, na_equal = na_equal)

    if (any(miss_x)) {
      y_extra <- seq_len(vec_size(y_key))[miss_x]
    }
  }

  list(x = x_loc, y = y_loc, y_extra = y_extra)
}

# TODO: Replace with `vec_unchop(x, ptype = integer())`
# once performance of `vec_c()` matches `unlist()`. See #4964.
index_flatten <- function(x) {
  unlist(x, recursive = FALSE, use.names = FALSE)
}
