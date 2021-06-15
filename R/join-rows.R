join_rows <- function(x_key,
                      y_key,
                      type = c("inner", "left", "right", "full"),
                      missing = "match",
                      condition = "==",
                      filter = "none",
                      multiple = "all") {
  type <- arg_match(type)

  if (type == "inner" || type == "right") {
    no_match <- "drop"

    if (missing == "propagate") {
      missing <- "drop"
    }
  } else {
    no_match <- NA_integer_
  }

  if (type == "right" || type == "full") {
    remaining <- NA_integer_
  } else {
    remaining <- "drop"
  }

  # Find matching rows in y for each row in x
  tryCatch(
    matches <- vctrs:::vec_matches(
      needles = x_key,
      haystack = y_key,
      condition = condition,
      filter = filter,
      missing = missing,
      no_match = no_match,
      remaining = remaining,
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

  list(x = matches$needles, y = matches$haystack)
}
