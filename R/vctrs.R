common_handler <- function(name) {
  function(cnd) {
    bullets <- c(
      glue("`{name}` must return compatible vectors across groups."),
      i = cnd_bullet_combine_details(cnd$x, cnd$x_arg),
      i = cnd_bullet_combine_details(cnd$y, cnd$y_arg)
    )
    abort(bullets, class = "dplyr:::error_incompatible_combine")
  }
}

dplyr_vec_cast_common <- function(chunks, name) {
  withCallingHandlers(
    vec_cast_common(!!!chunks),
    error = common_handler(name)
  )
}
dplyr_vec_ptype_common <- function(chunks, name) {
  withCallingHandlers(
    vec_ptype_common(!!!chunks),
    error = common_handler(name)
  )
}

# Version of `vec_size_common()` that takes a list.
# Useful for delaying `!!!` when used within an `expr()` call.
dplyr_list_size_common <- function(
  x,
  ...,
  size = NULL,
  absent = 0L,
  call = caller_env()
) {
  check_dots_empty0(...)
  vec_size_common(!!!x, .size = size, .absent = absent, .call = call)
}

# Version of `vec_recycle_common()` that takes a list.
# Useful for delaying `!!!` when used within an `expr()` call.
dplyr_list_recycle_common <- function(
  x,
  ...,
  size = NULL,
  call = caller_env()
) {
  check_dots_empty0(...)
  vec_recycle_common(!!!x, .size = size, .call = call)
}

dplyr_list_pall <- function(
  x,
  ...,
  missing = NA,
  size = NULL,
  error_call = caller_env()
) {
  check_dots_empty0(...)
  vec_pall(!!!x, .missing = missing, .size = size, .error_call = error_call)
}

dplyr_list_pany <- function(
  x,
  ...,
  missing = NA,
  size = NULL,
  error_call = caller_env()
) {
  check_dots_empty0(...)
  vec_pany(!!!x, .missing = missing, .size = size, .error_call = error_call)
}
