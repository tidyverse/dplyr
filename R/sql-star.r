star <- function() quote(`*`)
has_star <- function(x) any_apply(x, is.star)
is.star <- function(x) identical(x, star())
remove_star <- function(x) {
  if (is.null(x)) return(x)
  
  is_star <- vapply(x, is.star, logical(1))  
  x[!is_star]
}
