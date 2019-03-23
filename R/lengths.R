lengths <- function(x) {
  map_int(x, length)
}

compat_lengths <- function() {
  if (getRversion() >= "3.2.0") {
    rm("lengths", inherits = TRUE)
  }
}
