skip_if_no_lazy_character <- function() {
  skip_if(getRversion() <= "3.5.0")

  new_lazy_character <- import_vctrs("new_lazy_character", optional = TRUE)
  lazy_character_is_materialized <- import_vctrs("lazy_character_is_materialized", optional = TRUE)

  if (is.null(new_lazy_character) || is.null(lazy_character_is_materialized)) {
    skip("Lazy character helpers from vctrs are not available.")
  }

  invisible()
}

new_lazy_character <- function(fn) {
  f <- import_vctrs("new_lazy_character")
  f(fn)
}

lazy_character_is_materialized <- function(x) {
  f <- import_vctrs("lazy_character_is_materialized")
  f(x)
}
