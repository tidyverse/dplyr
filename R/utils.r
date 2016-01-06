compact <- function(x) Filter(Negate(is.null), x)

names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

"%||%" <- function(x, y) {
  if(is.null(x)) y else x
}

# is.atomic() is TRUE for atomic vectors AND NULL!
is_atomic <- function(x) {
  is.atomic(x) && !is.null(x)
}

is_1d <- function(x) {
  # dimension check is for matrices and data.frames
  (is_atomic(x) || is.list(x)) && length(dim(x)) <= 1
}
