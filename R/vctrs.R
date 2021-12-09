common_handler <- function(name){
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
