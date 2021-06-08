join_rows <- function(x_key,
                      y_key,
                      type = c("inner", "left", "right", "full"),
                      na_equal = TRUE,
                      condition = "==",
                      multiple = "all") {
  type <- arg_match(type)

  # Find matching rows in y for each row in x
  tryCatch(
    matches <- vctrs:::vec_matches(
      needles = x_key,
      haystack = y_key,
      condition = condition,
      na_equal = na_equal,
      multiple = multiple,
      nan_distinct = TRUE
    ),
    vctrs_error_incompatible_type = function(cnd) {
      rx <- "^[^$]+[$]"
      x_name <- sub(rx, "", cnd$x_arg)
      y_name <- sub(rx, "", cnd$y_arg)

      abort(c(
        glue("Can't join on `x${x_name}` x `y${y_name}` because of incompatible types."),
        i = glue("`x${x_name}` is of type <{x_type}>>.", x_type = vec_ptype_full(cnd$x)),
        i = glue("`y${y_name}` is of type <{y_type}>>.", y_type = vec_ptype_full(cnd$y))
      ))
    }
  )

  if (type == "right" || type == "inner") {
    # Drop rows that only exist in `x`
    if (anyNA(matches$haystack)) {
      matches <- vec_slice(matches, !vec_equal_na(matches$haystack))
    }
  }

  x_loc <- matches$needles
  y_loc <- matches$haystack
  y_extra <- integer()

  if (type == "right" || type == "full") {
    # Add rows from `y` not matching any `x` row
    miss_x <- !vec_in(vec_seq_along(y_key), y_loc)

    if (any(miss_x)) {
      y_extra <- which(miss_x)
    }
  }

  list(x = x_loc, y = y_loc, y_extra = y_extra)
}
