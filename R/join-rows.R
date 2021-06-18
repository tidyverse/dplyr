join_rows <- function(x_key,
                      y_key,
                      type = c("inner", "left", "right", "full"),
                      na_matches = "na",
                      condition = "==",
                      filter = "none",
                      multiple = "all",
                      check_unmatched = "neither",
                      check_duplicates = "neither") {
  type <- arg_match(type)

  missing <- standardise_join_missing(type, na_matches)
  no_match <- standardise_join_no_match(type, check_unmatched)
  remaining <- standardise_join_remaining(type, check_unmatched)
  check_duplicates <- standardise_join_check_duplicates(check_duplicates)

  matches <- dplyr_matches(
    needles = x_key,
    haystack = y_key,
    condition = condition,
    filter = filter,
    missing = missing,
    no_match = no_match,
    remaining = remaining,
    multiple = multiple,
    check_duplicates = check_duplicates
  )

  list(x = matches$needles, y = matches$haystack)
}

dplyr_matches <- function(needles,
                          haystack,
                          ...,
                          condition = "==",
                          filter = "none",
                          missing = "match",
                          no_match = NA_integer_,
                          remaining = "drop",
                          multiple = "all",
                          check_duplicates = "neither") {
  tryCatch(
    vctrs:::vec_matches(
      needles = needles,
      haystack = haystack,
      ...,
      condition = condition,
      filter = filter,
      missing = missing,
      no_match = no_match,
      remaining = remaining,
      multiple = multiple,
      check_duplicates = check_duplicates,
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
    },
    vctrs_error_matches_nothing = function(cnd) {
      i <- cnd$i

      abort(c(
        "Each row of `x` must have a match in `y`.",
        i = glue("Row {i} of `x` does not have a match.")
      ))
    },
    vctrs_error_matches_remaining = function(cnd) {
      i <- cnd$i

      abort(c(
        "Each row of `y` must be matched by `x`.",
        i = glue("Row {i} of `y` was not matched.")
      ))
    },
    vctrs_error_matches_duplicates = function(cnd) {
      i <- cnd$i

      if (cnd$needles) {
        input <- "x"
      } else {
        input <- "y"
      }

      abort(c(
        glue("`{input}` must not contain duplicate keys."),
        i = glue("Row {i} is a duplicate.")
      ))
    }
  )
}

standardise_join_missing <- function(type, na_matches) {
  if (na_matches == "na") {
    return("match")
  }

  if (type == "inner" || type == "right" || type == "semi") {
    return("drop")
  } else {
    return("propagate")
  }
}

standardise_join_no_match <- function(type, check_unmatched) {
  if (check_unmatched == "x" || check_unmatched == "both") {
    return("error")
  }

  if (type == "inner" || type == "right" || type == "semi") {
    return("drop")
  } else if (type == "nest") {
    return(0L)
  } else {
    return(NA_integer_)
  }
}

standardise_join_remaining <- function(type, check_unmatched) {
  if (check_unmatched == "y" || check_unmatched == "both") {
    return("error")
  }

  if (type == "right" || type == "full") {
    return(NA_integer_)
  } else {
    return("drop")
  }
}

standardise_join_check_duplicates <- function(check_duplicates) {
  switch(
    check_duplicates,
    neither = "neither",
    x = "needles",
    y = "haystack",
    both = "both"
  )
}
